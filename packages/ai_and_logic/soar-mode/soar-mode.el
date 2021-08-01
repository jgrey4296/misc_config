;;; soar-mode.el -*- lexical-binding: t; -*-
(require 'soar-faces)

(defcustom soar-executable "soar"
  "The executable to use for soar"
  :type "string")


(defvar-local soar-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst soar-font-lock-keywords
  (list
   `("#.+$" (0 'font-lock-comment-face)) ;; -- MOVE TO SYNTACTIC FONTIFICATION
   `("^\\(sp {\\)\\(.+\\)$"
     (1 'soar-face-1)
     (2 'soar-face-2))
   `(,(rx line-start ?})
     (0 'soar-face-3))
   `("-->" (0 'soar-face-0))
   `("<\\w+>" (0 'soar-face-1))
   `(,(rx ?^ (regexp "[-[:alpha:]]+")) (0 'soar-face-1))
   `(,(rx "(" (group (or "state" "impasse")))
     (1 'soar-face-2))
   `(,(rx ":" (or "o-support"
                  "i-support"
                  "chunk"
                  "default"))
     (0 'soar-face-2))
   `(,(rx (or "<<" ">>" ?{ ?}))
     (0 'soar-face-3))
   `(,(rx (or ?( ?) ))
     (0 'soar-face-0))
   `(,(rx (or "<>" ?< ?> "<=" ">=" "=" "<=>"))
     (0 'soar-face-1)
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
