;;; tag-timeline-mode.el -*- lexical-binding: t; -*-

(defvar-local tag-timeline-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst tag-timeline-font-lock-keywords
  (list)
  "Highlighting for tag-timeline-mode"
  )

(define-derived-mode tag-timeline-mode fundamental-mode
  "tag-timeline"
  "For Editing Tag Timelines"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tag-timeline-mode-map)

  (set (make-local-variable 'font-lock-defaults) (list tag-timeline-font-lock-keywords t))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'tag-timeline-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'tag-timeline-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table tag-timeline-mode-syntax-table)
  ;;
  (setq major-mode 'tag-timeline-mode)
  (setq mode-name "tag-timeline")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.tag_timeline" . tag-timeline-mode))

(provide 'tag-timeline-mode)
