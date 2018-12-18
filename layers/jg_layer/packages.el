;; jg_layer packages.el
;; loads second

(defconst jg_layer-packages
  '(
    ;; package from EPA
    ;; eg: some-package
    ;; (some-package :location elpa)
    ;; (some-package :location local)
    ;; (some-package :location (recipe :fetcher github :repo "some/repo"))
    ;;(some-package :excluded t)
    helm
    org
    yasnippet
    crosshairs
    abbrev
    evil
    smartparens
    )
  )

;; (defun <layer>/pre-init-<package>)
;; (defun <layer>/init-<package>)
;; (defun <layer>/post-init-<package>)

(defun jg_layer/post-init-evil ()
  ;; assign colours to hl-line based on state
  (add-hook 'evil-normal-state-entry-hook       (lambda () (set-face-background hl-line-face "#000000")))
  (add-hook 'evil-insert-state-entry-hook       (lambda () (set-face-background hl-line-face "#005f00")))
  (add-hook 'evil-visual-state-entry-hook       (lambda () (set-face-background hl-line-face "#005fff")))
  (add-hook 'evil-motion-state-entry-hook       (lambda () (set-face-background hl-line-face "#5f0000")))
  (add-hook 'evil-emacs-state-entry-hook        (lambda () (set-face-background hl-line-face "#5f00ff")))
  (add-hook 'evil-replace-state-entry-hook      (lambda () (set-face-background hl-line-face "#8700ff")))
  (add-hook 'evil-hybrid-state-entry-hook       (lambda () (set-face-background hl-line-face "#0087ff")))
  (add-hook 'evil-evilified-state-entry-hook    (lambda () (set-face-background hl-line-face "#5f5f00")))
  (add-hook 'evil-lisp-state-entry-hook         (lambda () (set-face-background hl-line-face "#875fff")))
  (add-hook 'evil-iedit-state-entry-hook        (lambda () (set-face-background hl-line-face "#8700af")))
  (add-hook 'evil-iedit-insert-state-entry-hook (lambda () (set-face-background hl-line-face "#8700af")))
  )

(defun jg_layer/post-init-helm ()
  ;;add in keybinding to kill line in completion window
  (define-key helm-map (kbd "C-K") 'kill-line)
  )


(defun jg_layer/post-init-org ()
  ;; add in keybinding to call tag-occurances
  (spacemacs/set-leader-keys
    "o T"     'org-todo-list
    "o t"     'jg_layer/tag-occurances
    "o v"     'org-tags-view
    "o a f"   'org-agenda-file-to-front
    "o a r"   'org-remove-file
    "o l f"   'list-agenda-files
    "o a l"   'org-agenda-list
    "o a w"   'org-agenda-week-view
    "o a m"   'org-agenda-month-view
    "o c"     'org-goto-calendar
    "o d"     'org-date-from-calendar
    "o D"     'org-time-stamp
    "o s c"   'org-edit-src-code
    "o l s"   'org-store-link
    "o l i"   'org-insert-link
    "o l d"   'org-toggle-link-display
    "o d"     'org-deadline
    "o t a"   'org-timestamp
    "o t i"   'org inactive timestamp
    "o s"     'org-schedule
    )
  )

(defun jg_layer/post-init-yasnippet ()
  (spacemacs/set-leader-keys
    "TAB"    'yas-expand
    "y y"    'yas-expand
    "y i"    'yas-insert-snippet
    "y n"    'yas-new-snippet
    "y d"    'yas-describe-tables
    )
  )

(defun jg_layer/post-init-abbrev ()
  (spacemacs/set-leader-keys
    "y e"  'edit-abbrevs
    "y w"  'write-abbrev-file
    "y r"  'read-abbrev-file
    "y a"  'add-global-abbrev
    "y A"  'add-mode-abbrev
    "y k"  'kill-all-abbrevs
    )
  )

(defun jg/layer/post-smartparens ()
  (setq-default smartparens-global-mode 0

                )
  )
