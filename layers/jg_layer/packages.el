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
    ibuffer
    neotree
    fci
    rainbow-mode
    )
  )

;; (defun <layer>/pre-init-<package>)
;; (defun <layer>/init-<package>)
;; (defun <layer>/post-init-<package>)

(defun jg_layer/post-init-evil ()
  (setq-default evil-escape-delay 0.3 )
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
  ;;ORG SETUP
  (setq-default
   org-agenda-files `(,(expand-file-name "~/github/jg_shell/shellnotes.org"))
   org-fast-tag-selection-single-key nil
   org-from-is-user-regexp "\\<John Grey\\>"
   org-group-tags nil
   org-use-fast-tag-selection t
   )
  ;; add in keybinding to call tag-occurances
  (spacemacs/declare-prefix "o" "Org")
  (spacemacs/declare-prefix "o t" "Tags")
  (spacemacs/declare-prefix "o a" "Agenda")
  (spacemacs/declare-prefix "o c" "Calendar")
  (spacemacs/declare-prefix "o s" "Source")
  (spacemacs/declare-prefix "o l" "Links")
  (spacemacs/set-leader-keys
    "o T"     'org-todo-list
    ;; TAGS
    "o t o"     'jg_layer/tag-occurances
    "o t v"     'org-tags-view
    ;; AGENDA
    "o a f"   'org-agenda-file-to-front
    "o a r"   'org-remove-file
    "o a l"   'org-agenda-list
    "o a w"   'org-agenda-week-view
    "o a m"   'org-agenda-month-view
    "o a f"   'list-agenda-files
    "o a d"   'org-deadline
    "o a s"     'org-schedule
    ;; CALENDAR
    "o c c"     'org-goto-calendar
    "o c d"     'org-date-from-calendar
    "o c t"     'org-time-stamp
    "o c i"     'org-inactive-timestamp
    ;; SRC CODE
    "o s c"   'org-edit-src-code
    ;; LINKS
    "o l s"   'org-store-link
    "o l i"   'org-insert-link
    "o l d"   'org-toggle-link-display
    )
  )

(defun jg_layer/post-init-yasnippet ()
  ;;yasnippet
  (setq-default yas-snippet-dirs `( ,(expand-file-name "~/.spacemacs.d/snippets/")
                                    ,(expand-file-name "~/github/otherLibs/yasnippet-snippets/snippets")
                                    ,(expand-file-name "~/github/otherLibs/yasnippet-snippets")))
  (spacemacs/declare-prefix "y" "Snippets/Abbrevs")
  (spacemacs/set-leader-keys
    "y y"    'yas-expand
    "y i"    'yas-insert-snippet
    "y n"    'yas-new-snippet
    "y d"    'yas-describe-tables
    )
  )

(defun jg_layer/post-init-abbrev ()
  ;;abbrev-file complaint quieting
  (setq-default
   abbrev-file-name (expand-file-name "~/.spacemacs.d/layers/jg_layer/abbrevs_defs")
   )
  (spacemacs/set-leader-keys
    "y e"  'edit-abbrevs
    "y w"  'write-abbrev-file
    "y r"  'read-abbrev-file
    "y a"  'add-global-abbrev
    "y A"  'add-mode-abbrev
    "y k"  'kill-all-abbrevs
    )
  (global-set-key (kbd "TAB") 'expand-abbrev)
  )

(defun jg_layer/post-smartparens ()
  (setq-default smartparens-global-mode 0
                )
  )

(defun jg_layer/post-ibuffer ()
  )

(defun jg_layer/post-erlang ()
  ;; (also has a load path set in root el file)
  erlang-root-dir "/usr/local/opt/erlang"
  exec-path (cons "/usr/local/opt/erlang/bin" exec-path)

  )

(defun jg_layer/post-python ()
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset nil )
  )

(defun jg_layer/post-fci ()
  (add-hook 'change-major-mode-after-body-hook 'fci-mode)
  )

(defun jg_layer/init-rainbow-mode ()
  (use-package rainbow-mode
    :commands (rainbow-mode)
    )
  )

(defun jg_layer/post-init-rainbow-mode ()
  (rainbow-mode t)
  )
