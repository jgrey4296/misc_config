;;; +faces.el -*- lexical-binding: t; -*-

(defun +jg-ui-state-line-change ()
  (hl-line-unhighlight)
  (setq hl-line-face (cond ((eq evil-state 'insert)  'jg-insert-line)
                           ((eq evil-state 'visual)  'jg-visual-line)
                           ((eq evil-state 'motion)  'jg-motion-line)
                           ((eq evil-state 'replace) 'jg-replace-line)
                           ((eq evil-state 'iedit)   'jg-iedit-line)
                           (t 'jg-normal-line)))
  (hl-line-highlight)
  )


;; Set up faces for hl-line colour sync to status
(defface jg-normal-line       #'((t :background "#000000")) "The Evil Normal State Hl-line")
(defface jg-insert-line       #'((t :background "#005f00")) "The Evil Insert State Hl-line")
(defface jg-visual-line       #'((t :background "#005fff")) "The Evil Visual State Hl-line")
(defface jg-motion-line       #'((t :background "#5f0000")) "The Evil Motion State Hl-line")
(defface jg-iedit-line        #'((t :background "#8700af")) "The Evil iedit State Hl-line")
(defface jg-iedit-insert-line #'((t :background "#8700af")) "The Iedit Insert state Hl-line")

(defface jg-emacs-line        #'((t :background "#5f00ff")) "The Evil Emacs State Hl-line")
(defface jg-replace-line      #'((t :background "#8700ff")) "The Evil Replace State Hl-line")
(defface jg-hybrid-line       #'((t :background "#0087ff")) "The Evil Hybrid State Hl-line")
(defface jg-evilified-line    #'((t :background "#5f5f00")) "The Evil Evilified State Hl-line")
(defface jg-lisp-line         #'((t :background "#875fff")) "The Evil Lisp State Hl-line")

(after! (evil hl-line)
  ;; hooks for evil state entry hooks to change hl-line colour
  (add-hook! '(evil-normal-state-entry-hook
               evil-insert-state-entry-hook
               evil-visual-state-entry-hook
               evil-motion-state-entry-hook
               evil-emacs-state-entry-hook
               evil-replace-state-entry-hook
               evil-hybrid-state-entry-hook
               evil-evilified-state-entry-hook
               evil-lisp-state-entry-hook
               evil-iedit-state-entry-hook) #'+jg-ui-state-line-change)
  )


;;-- end hl-line control
