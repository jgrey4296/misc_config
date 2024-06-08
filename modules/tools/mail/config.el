;;; util/jg-mail/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")

(function-put 'princ 'original (symbol-function 'princ))
(function-put 'princ 'mod (symbol-function '+jg-mail-princ-as-insert))

(use-package! mu4e
  :commands mu4e mu4e-compose-new
  ;; :hook     (mu4e-main-mode . +jg-mail-override-mu4e-hook)
  :init
  (provide 'html2text) ; disable obsolete package
  :config

  (setq mu4e-headers-threaded-label (cons "Thread view" "Thread View")
        mu4e-headers-related-label  (cons "Showing related emacs" "Showing related emacs")
        mu4e-headers-full-label     (cons "Search is Full" "Search is Full")
        )

  (plist-put (cdr (assoc :flags mu4e-header-info)) :shortname " Flags") ; default=Flgs
  (add-to-list 'mu4e-bookmarks '("flag:flagged" "Flagged messages" ?f) t)
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser))   ;; Html mails might be better rendered in a browser
  (when (fboundp 'make-xwidget) (add-to-list 'mu4e-view-actions '("xwidgets view" . mu4e-action-view-in-xwidget)))
  (when (fboundp 'imagemagick-register-types) (imagemagick-register-types))

  (setq mu4e-header-info-custom +mu4e-header-info-custom)

  ;;-- advice
  ;; Marks usually affect the current view
  (advice-add 'mu4e-mark-execute-all  :after   #'+mu4e--refresh-current-view-a)
  (advice-add 'mu4e-compose-new       :before  #'+mu4e-ensure-compose-writeable-a)
  (advice-add 'mu4e-compose-reply     :before  #'+mu4e-ensure-compose-writeable-a)
  (advice-add 'mu4e-compose-forward   :before  #'+mu4e-ensure-compose-writeable-a)
  (advice-add 'mu4e-compose-resend    :before  #'+mu4e-ensure-compose-writeable-a)
  (advice-add #'mu4e--key-val         :filter-return #'+mu4e~main-keyval-str-prettier-a)
  (advice-add #'mu4e--main-action-str :override #'+mu4e~main-action-str-prettier-a)
  (advice-add 'mu4e--start            :around #'+mu4e-lock-start)
  (advice-add 'mu4e-quit              :after #'+mu4e-lock-file-delete-maybe)

  ;;-- end advice

  ;;-- hooks
  (add-hook 'kill-emacs-hook #'+mu4e-lock-file-delete-maybe)
  (add-hook 'mu4e-compose-pre-hook '+mu4e-set-from-address-h)
  (add-hook 'message-send-hook #'+mu4e-check-for-subject)
  (add-hook 'mu4e-headers-mode-hook #'+mu4e-widen-frame-maybe)
  ;; Wrap text in messages
  (setq-hook! 'mu4e-view-mode-hook truncate-lines nil)
  ;;-- end hooks

  (local-load! "+gmail")
  )

(use-package! rmail
  :commands rmail
  :after evil
  :config

  ;;-- advice
  (advice-add 'rmail-header-summary :override #'+jg-mail-header-summary)
  (advice-add 'rmail-create-summary :override #'+jg-mail-create-summary)
  (advice-add 'rmail-new-summary-1 :around #'+jg-mail-new-summary-princ-override)
  (advice-add 'rmail-summary-update-line :around #'+jg-mail-summary-update-princ-override)
  ;;-- end advice
)

(use-package! rmailsum
  :after rmail
  )
