;;; cron-mode.el -*- lexical-binding: t; -*-

(defvar-local cron-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst cron-mode-font-lock-keywords
  (list
   ;; Basic Assigments
   '("^\\([A-Z_]+\\)=\\(.+\\)"
     (1 "custom-changed")
     (2 "custom-button-unraised"))
   ;; Comments
   '("^# .+"
     (0 "custom-face-tag"))
   ;; Headers
   '("^## .+"
     (0 "custom-set"))
   ;; Headers 2
   '("^### .+"
     (0 "custom-modified"))
  ;; Match Days
   '(" \\(mon\\|tue\\|wed\\|thu\\|fri\\|sat\\|sun\\) "
     (1 "font-lock-warning-face")
     )

  ;; Match actual cron specs
   (list (let ((pat "[[:word:]*,/]+"))
           (format "^\\(%s %s %s %s\\) %s \\(.+\\)$" pat pat pat pat pat))
         '(1 "custom-saved")
         '(2 "font-lock-type-face")
     )

   )
  "Highlighting for cron-mode"
)

(define-derived-mode cron-mode fundamental-mode
  "Cron-mode"
  "A Simple mode for editing cron specifications"
  (interactive)
  (kill-all-local-variables)
  (use-local-map cron-mode-map)

  (set (make-local-variable 'font-lock-defaults) (list cron-mode-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'cron-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'cron-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table cron-mode-syntax-table)
  ;;
  (setq major-mode 'cron-mode)
  (setq mode-name "cron")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )

(add-to-list 'auto-mode-alist '("/crontab\\..+" . cron-mode))

(provide 'cron-mode)
