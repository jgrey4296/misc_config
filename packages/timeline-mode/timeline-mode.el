;;; timeline-mode.el -*- lexical-binding: t; -*-

(defvar-local timeline-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst timeline-font-lock-keywords
  (list)
  "Highlighting for timeline-mode"
  )

(define-derived-mode timeline-mode fundamental-mode
  "timeline"
  "For Editing Timeline Specs"
  (interactive)
  (kill-all-local-variables)
  (use-local-map timeline-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list timeline-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'timeline-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'timeline-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table timeline-mode-syntax-table)
  ;;
  (setq major-mode 'timeline-mode)
  (setq mode-name "timeline")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.timeline" . timeline-mode))

(provide 'timeline-mode)
