;; -*- mode: elisp; lexical-binding: t; -*-
;;
(provide 'acab-rule-mode)

;; TODO a mode for authoring and editing acab rules
;;
;;INSERTION
(defun acab-ide/el-string-helm ()
  (interactive)
  ;;TODO el string helm
  )
(defun acab-ide/insert-tag ()
  (interactive)
  ;;TODO insert tag
  ;;select rule window
  ;;find tags
  ;;populate tags
  ;;run tag helm

  )
(defun acab-ide/insert-transform ()
  ;;TODO insert transform
  ;;select rule window
  ;;find tags
  ;;populate tags
  ;;run transform helm

  )
(defun acab-ide/insert-action ()
  ;;TODO insert action
  ;;select rule window
  ;;find tags
  ;;populate tags
  ;;run action helm
  )
(defun acab-ide/insert-from-side-buffer ()
  ;;TODO insert from side buffer
  )

(defvar-local acab-rule-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst acab-rule-font-lock-keywords
  (list)
  "Highlighting for acab-rule-mode"
  )

(define-derived-mode acab-rule-mode fundamental-mode
  "acab-rule"
  "Mode for authoring Acab Rule Structures"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acab-rule-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list acab-rule-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'acab-rule-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'acab-rule-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table acab-rule-mode-syntax-table)
  ;;
  (setq major-mode 'acab-rule-mode)
  (setq mode-name "acab-rule")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.rule" . acab-rule-mode))
