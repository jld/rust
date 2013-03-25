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
  `(
    ("\\_<\\(fn\\|mod\\|use\\)[[:space:]]+\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\_>" 2 font-lock-function-name-face)
    ("\\_<let[[:space:]]+\\(mut[[:space:]]+\\)?\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\_>" 2 font-lock-variable-name-face)
    ("\\_<\\(type\\|struct\\|enum\\)[[:space:]]+\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\_>" 2 font-lock-type-face)

    (,(regexp-opt '("mod" "type" "struct" "fn" "enum" "impl"
		    "as" "break" "copy" "do" "else" "extern" "for" "if"
		    "match" "let" "loop" "once""priv" "pub" "ref" "return" 
		    "static" "unsafe" "use" "while" "mut") 'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt '("int" "uint" "i64" "u64" "i32" "u32" "i16" "u16"
		    "i8" "u8" "char" "bool") 'symbols)
     . font-lock-builtin-face)
    (,(regexp-opt '("self" "true" "false") 'symbols)
     . font-lock-constant-face)

    ("'\\([^']\\|\\\\\\([ntr\"'\\\\]\\|x[0-9A-Fa-f][0-9A-Fa-f]\\|u[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\|U[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\)\\)'" . font-lock-string-face)

    ("\\_<\\([a-zA-Z_][a-zA-Z_0-9]*\\)::" 1 font-lock-builtin-face)
    ("\\([a-zA-Z_][a-zA-Z_0-9]*\\):" 1 font-lock-variable-name-face)
    ("\\_<[A-Z][a-zA-Z_0-9]*\\_>" . font-lock-type-face)
    ("\\_<[a-zA-Z_][a-zA-Z_0-9]*!\\|#\\(\\[[^]]*\\]\\)?" 0 font-lock-preprocessor-face t)

    ("\\_<[0-9][bx]?[0-9_]*\\([ui][0-9]*\\|\\(\\.[0-9_]+\\)?\\([eE][-+]?[0-9_]+\\)?\\(f[0-9]+\\)?\\)\\_>" . font-lock-constant-face)

    ))


;;;###autoload
(define-derived-mode new-rust-mode prog-mode "New Rust"
  "Major mode for editing source code in the Rust language."
  (set-syntax-table new-rust-syntax-table)
  (run-hooks 'new-rust-mode-hook)
  (set (make-local-variable 'font-lock-defaults) 
       '(new-rust-font-lock-keywords))
  (set (make-local-variable 'fill-paragraph-function)
       'fill-comment-paragraph)
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\s *")
  (set (make-local-variable 'comment-end) " */")

  (set (make-local-variable 'comment-style) 'extra-line) ; controversial?
  (set (make-local-variable 'comment-multi-line) t)

  (setq fill-column new-rust-fill-column)
)

(provide 'new-rust-mode)
