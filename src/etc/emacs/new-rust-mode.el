(require 'cc-mode)
(eval-when-compile (require 'cl))

(defvar new-rust-mode-hook nil)
(defvar new-rust-indent-unit 4)
(defvar new-rust-fill-column 100)
(defvar new-rust-debug-indent nil)
(defvar new-rust-electric-flag t)


(defvar new-rust-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    ;; ' is used for region variables, so its syntax class depends on context.
    ;; (See the syntactic font lock entry below.)
    (modify-syntax-entry ?' "." table)
    table))

(defconst new-rust-font-lock-syntactics
  ;; Delimiterize single quotes for char literals (and not region variables).
  '(("\\('\\)\\([^'\\]\\|\\\\\\([ntr\"'\\\\]\\|x[0-9A-Fa-f]\\{2\\}\\|u[0-9A-Fa-f]\\{4\\}\\|U[0-9A-Fa-f]\\{8\\}\\)\\)\\('\\)" (1 "\"") (4 "\""))))

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
;;
;; FIXME: This is not very helpful if run inside a block comment.

(defun new-rust-indent-line ()
  (interactive)
  (let ((orig-bol (point-at-bol)) target)
    (when (> (point-at-bol) (point-min))
      (save-excursion
	(forward-to-indentation 0)
	;; Handle a leading close delimiter specially.
	;; (Only the first, for least surprise.)
	(when (eq (syntax-class (syntax-after (point))) 5)
	  (forward-char)
	  (backward-sexp)
	  ;; If it was expression-like, indent to match the open paren.
	  (when (new-rust-stuff-afterp (+ (point) 1))
	    (setq target (- (point) (point-at-bol)))))
	(unless target
	  (save-excursion
	    (let ((adjust 0) (ignore-open-after (point-at-bol))
		  syn sc indent-point no-dedent)
	      (loop
	       until (and (<= (point) (point-min))
			(setq target 0))
	       do (setq syn (syntax-after (- (point) 1))
			sc (syntax-class syn)
			;; XXX this is confusingly wrong if tabs get in
			indent-point (+ (point-at-bol) (current-indentation)))
	       ;; If at bol and any non-ws on line, use cur-indent.
	       until (and (<= (point) indent-point)
			  (< (point) orig-bol)
			  (re-search-forward "[^[:space:]]" (point-at-eol) t)
			  (progn (backward-char) t)
			  (setq target (- (point) (point-at-bol))))
	       ;; If at open delimiter, it's complicated:
	       until (and (or (and (eq sc 4)
				   (< (point) ignore-open-after))
			      ;; Treat beginning of multiline string as open.
			      (and (eq sc 7)
				   (eq (syntax-class
					(syntax-after
					 (- (point-at-eol) 1))) 9)))
			  ;; Ignore other opens on this line.
			  (setq ignore-open-after (point-at-bol))
			  ;; Anything between it and eol but comments/space?
			  (let ((found-nonspace (new-rust-stuff-afterp)))
			    (if found-nonspace
			      (save-excursion
				;; If so, indent to first nonspace.
				;; (Which might be a comment.)
				(setq target
				      (progn (goto-char found-nonspace)
					     (- (point) (point-at-bol)))
				      ;; If it's a continuation, then so be it.
				      no-dedent t))
			      ;; Otherwise, indent one level and continue.
			      (progn
				(incf adjust new-rust-indent-unit)
				nil))))
	       ;; Otherwise, step back one thing.
	       do (or
		   ;; Skip one comment (but only one) or whitespace.
		   ;; (If it's a comment, we'll land on the first char.)
		   (let ((p (point)))
		     (forward-comment -1)
		     (/= p (point)))
		   ;; If we're at a non-comment-or-space:
		   (case sc
		     ;; Skip over a delimited group or a string.
		     ;; Also skip an entire symbol at once for efficiency.
		     ;;
		     ;; Note: we can't be inside a string here, because (see
		     ;; above) it will always have nonspace after the
		     ;; delimiter, for the backslash if nothing else.
		     ((2 3 5 7) (backward-sexp))
		     (otherwise (backward-char)))))
	      (when new-rust-debug-indent
		(message "ref=%d target=%d adjust=%d" (point) target adjust))
	      (incf target adjust)
	      ;; Dedent if our reference is a continuation.
	      (unless (or no-dedent (new-rust-thing-startp (point)))
		(decf target new-rust-indent-unit))))
	  ;; Indent if we're a continuation.
	  (unless (new-rust-thing-startp (point))
	    (incf target new-rust-indent-unit)))))
    ;; At long last, actually indent.  But, if we were after the indentation,
    ;; don't move the cursor relative to the text.
    (let ((init-col (- (point) (point-at-bol) (current-indentation))))
      (indent-line-to (or target 0))
      (when (> init-col 0)
	(goto-char (+ (point-at-bol) (current-indentation) init-col))))))

;; Anything between `pt' (inclusive) and eol but comments/space?
;; If so, return the point that non-whitespace starts.
(defun new-rust-stuff-afterp (&optional pt)
  (save-excursion
    (when pt (goto-char pt))
    (let ((found-nonspace
	   (re-search-forward "[^[:space:]]"
			      (point-at-eol) t)))
      (when found-nonspace
	;; Need the beginning of the match, not the end.
	(backward-char)
	(setq found-nonspace (point))
	;; Note that skipping comments may end on a different line.
	;; This is intentional.
	(while (forward-comment 1))
	;; Have we found something other than the start
	;; of a new line or the end of the buffer?
	(and (< (point) (point-max))
	     (< (current-indentation)
		(- (point) (point-at-bol)))
	     found-nonspace)))))

;; Does the line containing `pt' start a new thing, or does it need to
;; be indented as a continuation?
(defun new-rust-thing-startp (&optional pt)
  (save-excursion
    (when pt (goto-char pt))
    (forward-to-indentation 0)
    (or
     (memq (char-after) '(?{ ?}))
     (progn
       (while (forward-comment -1))
       (or
	(<= (point) (point-min))
	(case (char-before)
	  ((?, ?\; ?} ?\( ?\[ ?{ ?\\) t)
	  (?\] ;; Is this an #[attribute]?
	   (backward-sexp)
	   (while (forward-comment -1))
	   (eq (char-before) ?#))
	  (otherwise nil)))))))


;; Electrically insert a syntactically important character.
(defun new-rust-electric-insert (arg)
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (when (and new-rust-electric-flag (not arg))
    (new-rust-indent-line)))

;;;###autoload
(define-derived-mode new-rust-mode prog-mode "New Rust"
  "Major mode for editing source code in the Rust language."
  (set-syntax-table new-rust-syntax-table)
  (run-hooks 'new-rust-mode-hook)
  (set (make-local-variable 'font-lock-defaults)
       `(new-rust-font-lock-keywords
	 nil nil nil nil
	 (font-lock-syntactic-keywords
	  . ,new-rust-font-lock-syntactics)))

  (dolist (ch '(?( ?) ?[ ?] ?{ ?} ?\; ?,))
    (local-set-key (vector ch) 'new-rust-electric-insert))

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

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.rs$" . new-rust-mode))
  (add-to-list 'auto-mode-alist '("\\.rc$" . new-rust-mode)))

(provide 'new-rust-mode)
