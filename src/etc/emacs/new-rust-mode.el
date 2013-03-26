(require 'cc-mode)
(eval-when-compile (require 'cl))

(defvar new-rust-mode-hook nil)
(defvar new-rust-indent-unit 4)
(defvar new-rust-fill-column 100)


(defvar new-rust-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?' "." table)
    table))

(defconst new-rust-font-lock-keywords
  ;; FIXME: this will not get non-ASCII code right, but I don't
  ;; understand Emacs regexp well enough to fix it.
  `(
    ;; Keywords:
    (,(regexp-opt '("mod" "type" "struct" "fn" "enum" "impl"
		    "as" "break" "copy" "do" "else" "extern" "for" "if"
		    "match" "let" "loop" "once" "priv" "pub" "ref" "return"
		    "static" "unsafe" "use" "while" "mut") 'symbols)
     . font-lock-keyword-face)

    ;; Character constants (and not region variables). FIXME: syntactic-ize this so that '(' doesn't match ).
    ("'\\([^']\\|\\\\\\([ntr\"'\\\\]\\|x[0-9A-Fa-f]\\{2\\}\\|u[0-9A-Fa-f]\\{4\\}\\|U[0-9A-Fa-f]\\{8\\}\\)\\)'" . font-lock-string-face)

    ;; Macro names:
    ("\\_<[a-zA-Z_][a-zA-Z_0-9]*!" 0 font-lock-preprocessor-face)
    ;; Module names:
    ("\\_<\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\s *::" 1 font-lock-builtin-face)
    ;; Typed names -- fn args, struct fields, statics, etc.:
    ("\\_<\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\s *:" 1 font-lock-variable-name-face)
    ;; CamelCase (and EMPHATIC_CONSTANT) names:
    ("\\_<[A-Z][a-zA-Z_0-9]*\\_>" . font-lock-type-face)

    ;; Directives; nested instances of higher-priority syntax (e.g.,
    ;; strings and CamelCase) will not prevent this from matching, but
    ;; will retain their regular markup.  Multi-line directives may not
    ;; be recognized due to limitations of font-lock.
    ("#\\[[^]]*\\]" 0 font-lock-preprocessor-face keep)

    ;; Numeric constants. More permissive than the spec, but I think it agrees within the set of valid programs.
    ("\\_<[0-9][bx]?[0-9_]*\\([ui][0-9]*\\|\\(\\.[0-9_]+\\)?\\([eE][-+]?[0-9_]+\\)?\\(f[0-9]+\\)?\\)\\_>" . font-lock-constant-face)

    ;; Identifiers being bound, relatively early in the list for cases like `mod int`.
    ;; `static` is a deliberate omission; see above for type ascriptions and camel case.
    ;; `use` could be added, but would need to skip the leading path.
    ("\\_<\\(fn\\|mod\\)[[:space:]]+\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\_>" 2 font-lock-function-name-face)
    ("\\_<let[[:space:]]+\\(mut[[:space:]]+\\)?\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\_>" 2 font-lock-variable-name-face)
    ("\\_<\\(type\\|struct\\|enum\\)[[:space:]]+\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\_>" 2 font-lock-type-face)

    ;; Semi-reserved words:
    (,(regexp-opt '("int" "uint" "i64" "u64" "i32" "u32" "i16" "u16"
		    "i8" "u8" "char" "bool") 'symbols)
     . font-lock-builtin-face)
    (,(regexp-opt '("self" "true" "false") 'symbols)
     . font-lock-constant-face)
    ))


(defun new-rust-fill-paragraph (&optional justify)
  ;; XXX the searches in here do the wrong thing in some cases.
  ;; Also this is far too complicated-looking for what it does.
  ;; Furthermore, it behaves suboptimally if run outside a comment.
  (interactive "*P")
  (save-excursion
    (beginning-of-line)
    (let* ((start (point))
	   (end (point-at-eol))
	   (is-line-comments (prog1 (search-forward "//" end t)
			       (beginning-of-line))))
      (cond
       (is-line-comments
	;; Skip ahead over lines that are just //
	(beginning-of-line)
	(while (and (not (re-search-forward "[^[:space:]/]" (point-at-eol) t))
		    (search-forward "//" (point-at-eol) t)
		    (zerop (forward-line 1))))
	;; If that moved us forward, then we know where the start is.
	(if (/= start (point-at-bol))
	    (setq start (point-at-bol))
	  ;; Otherwise, find the beginning.
	  (save-excursion
	    (beginning-of-line)
	    (while (and (zerop (forward-line -1))
			(re-search-forward "[^[:space:]]" (point-at-eol) t)
			(eq (char-before) ?/)
			(eq (char-after) ?/)
			(re-search-forward "[^[:space:]/]" (point-at-eol) t)
			(setq start (point-at-bol))))))
	;; Find the end of the comment or paragraph
	(while (and (zerop (forward-line 1))
		    (re-search-forward "[^[:space:]]" (point-at-eol) t)
		    (eq (char-before) ?/)
		    (eq (char-after) ?/)
		    (re-search-forward "[^[:space:]/]" (point-at-eol) t)
		    (setq end (point-at-eol)))))
       ;; Otherwise, it's a block comment:
       (t
	;; Skip ahead over lines that are blank or just a *.
	(while (and (not (re-search-forward "[^[:space:]*]" (point-at-eol) t))
		    (zerop (forward-line 1))))
	;; If that moved us forward, then we know where the start is.
	(if (/= start (point-at-bol))
	    (setq start (point-at-bol))
	  ;; Otherwise, find the beginning.
	  (save-excursion
	    (beginning-of-line)
	    (while (and (not (search-forward "/*" (point-at-eol) t))
			(re-search-forward "[^[:space:]*]" (point-at-eol) t)
			(zerop (forward-line -1))))
	    ;; Use the first line only if it's more than just /*
	    (beginning-of-line)
	    (if (re-search-forward "[^[:space:]*/]" (point-at-eol) t)
		(setq start (point-at-bol))
	      (setq start (point-at-eol)))))
	;; Find the end of the comment or paragraph
	(beginning-of-line)
	(while (and (not (search-forward "*/" (point-at-eol) t))
			(re-search-forward "[^[:space:]*]" (point-at-eol) t)
			(zerop (forward-line 1))))
	;; Use the last line only if it's more than just */
	(beginning-of-line)
	(if (re-search-forward "[^[:space:]*/]" (point-at-eol) t)
	    (setq end (point-at-eol))
	  (setq end (point-at-bol)))))
      ;; Finally, do the actual fill.
      (let ((adaptive-fill-regexp ; NOTE dynamic scope let
	     (concat
	      (if is-line-comments "\\(\s *//+\\)?" "")
	      adaptive-fill-regexp)))
	(fill-region start end justify)))))


;; The indent function.  Our intuitions here are:
;;
;; * The indentation of a line is the indentation of the point after
;;   any leading closing delimeters.
;;
;; * Two points belonging to the same node of the token tree should
;;   have the same indentation.
;;
;; As applied to these slightly fuzzy rules:
;;
;; * An open delimiter at the end of the line (modulo whitespace and
;;   comments) indents by one unit relative to its context; otherwise,
;;   the contents are indented to the starting column of that
;;   non-space text.
;;
;; * If a line has multiple open delimiters, only the last takes effect.
;;
;; * A thing which is normally delimited with a semicolon or comma
;;   (statement, expression, etc.), if continued over multiple lines,
;;   should indent lines after the first by one unit.
;;
;; So this function, which attempts to indent a line by reference to
;; previous lines, walks backwards, stepping over sexps, until it
;; finds a line it can use as a reference for the current one.

(defun new-rust-indent-line ()
  (interactive)
  (let ((target 0) end-of-space end-of-close)
    (when (> (point-at-bol) (point-min))
      (save-excursion
	(beginning-of-line)
	(while (zerop (syntax-class (syntax-after (point)))) ; space
	  (forward-char))
	(setq end-of-space (point))
	(while (eq (syntax-class (syntax-after (point))) 5) ; close
	  (forward-char))
	(setq end-of-close (point))
	(let ((limit (save-excursion
		       (beginning-of-line)
		       (re-search-backward "[^[:space:]]" (point-min) 'move)
		       (point)))
	      close-paren open-paren)
	  ;; The goal: reach the start of a line, no later than the
	  ;; previous non-empty line, when there isn't a deferred
	  ;; backward-sexp left to run.
	  (while (or (> (point) (point-at-bol))
		     (>= (point) limit)
		     close-paren)
	    (let ((sc (syntax-class (syntax-after (- (point) 1)))))
	      (cond
	       ((and close-paren (/= sc 0)) ; not space and inside sexp
		(goto-char close-paren)
		(setq close-paren nil)
		(backward-sexp))
	       ((eq sc 4) ; open
		(backward-char)
		(if (not open-paren)
		    (setq open-paren (point))))
	       ((eq sc 5) ; close
		(if (not close-paren)
		    (setq close-paren (point)))
		(backward-char))
	       ((eq sc 7) ; string delimiter
		;; FIXME: this is wrong if we were inside a multiline string.
		;; (How do we detect that? Look for class 9 before 12?)
		(backward-sexp))
	       ((or (eq sc 1) (eq sc 12)) ; punctuation or newline
		;; That should be a test for the comment endings
		;; flags, not classes.  This attempts to skip over a
		;; comment, or just the character if it wasn't:
		(if (= (prog1 (point) (forward-comment -1)) (point))
		    (backward-char)))
	       (t
		(backward-char)))))
	  (let ((ref-indent (current-indentation)))
	    (message "point=%d ref-indent=%d open-paren=%s limit=%d"
		     (point) ref-indent open-paren limit)
	    (cond
	     (open-paren
	      (save-excursion
		(goto-char (+ open-paren 1))
		(while (forward-comment 1))
		(let ((thing-indent (- (point) (point-at-bol))))
		  ;; Did the open paren end its line? (mod space/comments)
		  (if (= thing-indent (current-indentation))
		      (setq target (+ ref-indent new-rust-indent-unit))
		    ;; FIXME: should we not skip comments to set the indent?
		    (setq target thing-indent)))))
	     ;; If we might be continuing, or not continuing, a thing:
	     ((= end-of-space end-of-close)
	      ;; Compare this line's continuedness to the reference.
	      (let ((delta (- (if (new-rust-proper-ending (point-at-bol)) 1 0)
			      (if (new-rust-proper-ending end-of-space) 1 0))))
		(setq target (+ ref-indent (* delta new-rust-indent-unit)))))
	     (t
	      (setq target ref-indent)))))))
    (indent-line-to target)))

(defun new-rust-proper-ending (pt)
  (save-excursion
    (goto-char pt)
    (while (forward-comment -1))
    (or
     (<= (point) (point-min))
     (case (syntax-class (syntax-after (- (point) 1)))
       ((4 5) t)
       ((1) (and (memq (char-before) '(?, ?\;)) t))
       (otherwise nil)))))


;;;###autoload
(define-derived-mode new-rust-mode prog-mode "New Rust"
  "Major mode for editing source code in the Rust language."
  (set-syntax-table new-rust-syntax-table)
  (run-hooks 'new-rust-mode-hook)
  (set (make-local-variable 'font-lock-defaults)
       '(new-rust-font-lock-keywords))

  (set (make-local-variable 'fill-paragraph-function)
       'new-rust-fill-paragraph)

  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\s *")
  (set (make-local-variable 'comment-end) " */")
  (set (make-local-variable 'comment-style) 'extra-line) ; controversial?
  (set (make-local-variable 'comment-multi-line) t)

  (setq indent-tabs-mode nil
	fill-column new-rust-fill-column)
  (set (make-local-variable 'indent-line-function) 'new-rust-indent-line)
)

(provide 'new-rust-mode)
