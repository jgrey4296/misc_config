;;; +modeline.el -*- lexical-binding: t; -*-

;; Define your custom doom-modeline
(doom-modeline-def-modeline 'my-simple-line
'(bar matches buffer-info remote-host buffer-position parrot selection-info)
'(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))

;; Set default mode-line
;; (add-hook 'doom-modeline-mode-hook
;;         (lambda ()
;;                 (doom-modeline-set-modeline 'my-simple-line 'default)))

;; Configure other mode-lines based on major modes
;; (add-to-list 'doom-modeline-mode-alist '(my-mode . my-simple-line))

;; Or disable other mode-lines
;; (setq 'doom-modeline-mode-alist nil)

(defun +jg-ui-modeline-choose ()
  (interactive)
  (doom-modeline-set-modeline
   'main
   ;; (intern-soft (ivy-read "Modeline: " doom-modeline-mode-alist))
   t)
  )
(doom-modeline 'minimal)
;; (doom-modeline-set-modeline 'magit-mode)
;; mode-line-format

(map! :map jg-help-map
      :prefix "u"
      "1" #'+jg-ui-modeline-choose
      )
