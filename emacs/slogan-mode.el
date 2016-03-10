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

(define-derived-mode slogan-mode java-mode
  "slogan mode"
  "Major mode for editing Slogan programs"
  (setq font-lock-defaults '((slogan-font-lock-keywords))))

(setq slogan-keywords nil)
(setq slogan-constants nil)
(setq slogan-keywords-regexp nil)
(setq slogan-constants-regexp nil)

(provide 'slogan-mode)
