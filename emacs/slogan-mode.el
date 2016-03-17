;; Need lot of work here...

(setq slogan-keywords '("fn" "function" "method" "define" "record"
                       "if" "else" "let" "letseq" "letrec" 
                       "case" "match" "where" "try" "trycc" "catch" "finally"
                       "macro" "namespace" "import" "declare"))
(setq slogan-constants '("true" "false"))

(setq slogan-keywords-regexp (regexp-opt slogan-keywords 'words))
(setq slogan-constant-regexp (regexp-opt slogan-constants 'words))

(setq slogan-font-lock-keywords
      `((,slogan-constant-regexp . font-lock-constant-face)
        (,slogan-keywords-regexp . font-lock-keyword-face)))

(defvar slogan-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for slogan-mode")
    
(define-derived-mode slogan-mode fundamental-mode "slogan mode"
  "Major mode for editing Slogan programs"
  :syntax-table slogan-mode-syntax-table
  (setq font-lock-defaults '((slogan-font-lock-keywords)))
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")
  (setq-local parse-sexp-ignore-comments t))

(setq slogan-keywords nil)
(setq slogan-constants nil)
(setq slogan-keywords-regexp nil)
(setq slogan-constants-regexp nil)

(provide 'slogan-mode)
