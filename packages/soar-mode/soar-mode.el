;;; soar-mode.el -*- lexical-binding: t; -*-

(defcustom soar-executable "soar"
  "The executable to use for soar"
  :type "string")


(defvar-local soar-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst soar-font-lock-keywords
  (list
   `("#.+$" (0 'font-lock-comment-face))
   `("^\\(sp {\\)\\(.+\\)$"
     (1 'font-lock-keyword-face)
     (2 'font-lock-function-name-face))
   `(,(rx line-start ?})
     (0 'font-lock-keyword-face))
   `("-->" (0 'font-lock-constant-face))
   `("<\\w+>" (0 'font-lock-variable-name-face))
   `(,(rx ?^ (regexp "[-[:alpha:]]+")) (0 'font-lock-type-face))
   `(,(rx "(" (group (or "state" "impasse")))
     (1 'font-lock-constant-face))
   `(,(rx ":" (or "o-support"
                  "i-support"
                  "chunk"
                  "default"))
     (0 'font-lock-warning-face))
   `(,(rx (or "<<" ">>" ?{ ?}))
     (0 '(:foreground "red")))
   `(,(rx (or ?( ?) ))
     (0 '(:foreground "green")))
   `(,(rx (or "<>" ?< ?> "<=" ">=" "=" "<=>"))
     (0 '(:foreground "green"))
     )
   )
  "Highlighting for soar-mode"
  )

(define-derived-mode soar-mode fundamental-mode
  "soar"
  "Major mode for use of soar"
  (interactive)
  (kill-all-local-variables)
  (use-local-map soar-mode-map)

  (set (make-local-variable 'font-lock-defaults) (list soar-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'soar-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'soar-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table soar-mode-syntax-table)
  ;;
  (setq major-mode 'soar-mode)
  (setq mode-name "soar")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.soar" . soar-mode))

(provide 'soar-mode)
