;;; util/jg-mail/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")

(use-package! mu4e
  :commands mu4e mu4e-compose-new
  :hook     (mu4e-main-mode . +jg-mail-override-mu4e-hook)
  :init
  (provide 'html2text) ; disable obsolete package
  :config
  (pcase +mu4e-backend
    (`mbsync      (setq mu4e-get-mail-command "mbsync -a"
                        mu4e-change-filenames-when-moving t))
    (`offlineimap (setq mu4e-get-mail-command "offlineimap -o -q"))
    )

  ;; Better search symbols
  (letf! ((defun make-help-button (text help-echo)
            (with-temp-buffer (insert-text-button text
                                                  'help-echo help-echo
                                                  'mouse-face nil)
                              (buffer-string)))
          (defun make-help-button-cons (text1 text2 help-echo)
            (cons (make-help-button text1 help-echo)
                  (make-help-button text2 help-echo))))
    (setq mu4e-headers-threaded-label
          (make-help-button-cons "T" (concat " " (all-the-icons-octicon "git-branch" :v-adjust 0.05))
                                 "Thread view")
          mu4e-headers-related-label
          (make-help-button-cons "R" (concat " " (all-the-icons-material "link" :v-adjust -0.1))
                                 "Showing related emails")
          mu4e-headers-full-label
          (make-help-button-cons "F" (concat " " (all-the-icons-material "disc_full"))
                                 "Search is full!")))

  (plist-put (cdr (assoc :flags mu4e-header-info)) :shortname " Flags") ; default=Flgs
  (add-to-list 'mu4e-bookmarks '("flag:flagged" "Flagged messages" ?f) t)
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser))   ;; Html mails might be better rendered in a browser
  (when (fboundp 'make-xwidget) (add-to-list 'mu4e-view-actions '("xwidgets view" . mu4e-action-view-in-xwidget)))
  (when (fboundp 'imagemagick-register-types) (imagemagick-register-types))

  (setq mu4e-header-info-custom +mu4e-header-info-custom)

  ;; Marks usually affect the current view
  (defadvice! +mu4e--refresh-current-view-a (&rest _)
    :after #'mu4e-mark-execute-all (mu4e-search-rerun))
  (defadvice! +mu4e-ensure-compose-writeable-a (&rest _)
    "Ensure that compose buffers are writable.
This should already be the case yet it does not always seem to be."
    :before #'mu4e-compose-new
    :before #'mu4e-compose-reply
    :before #'mu4e-compose-forward
    :before #'mu4e-compose-resend
    (read-only-mode -1))

  (advice-add #'mu4e--key-val :filter-return #'+mu4e~main-keyval-str-prettier-a)
  (advice-add #'mu4e--main-action-str :override #'+mu4e~main-action-str-prettier-a)
  (advice-add 'mu4e--start :around #'+mu4e-lock-start)
  (advice-add 'mu4e-quit :after #'+mu4e-lock-file-delete-maybe)

  (add-hook 'kill-emacs-hook #'+mu4e-lock-file-delete-maybe)
  (add-hook 'mu4e-compose-pre-hook '+mu4e-set-from-address-h)

  ;; Wrap text in messages
  (setq-hook! 'mu4e-view-mode-hook truncate-lines nil)

  (local-load! "+gmail")
  )

(use-package! rmail
  :commands rmail
  :after evil
  )

(use-package! rmailsum
  :after rmail
  )
