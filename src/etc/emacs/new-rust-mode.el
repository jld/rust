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

  (setq fill-column new-rust-fill-column)
)

(provide 'new-rust-mode)
