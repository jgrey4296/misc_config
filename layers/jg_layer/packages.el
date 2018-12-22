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
    ;; crosshairs
    abbrev
    evil
    (smartparens :excluded t)
    ibuffer
    ;; neotree
    fci
    rainbow-mode
    dired
    )
  )

;; (defun <layer>/pre-init-<package>)
;; (defun <layer>/init-<package>)
;; (defun <layer>/post-init-<package>)

(defun jg_layer/post-init-evil ()
  (setq-default evil-escape-delay 0.3 )
  (global-set-key (kbd "<backtab>") 'evil-normal-state)

  (defface evil-normal-state '((t :background  "#000000")) "The Evil Normal State Hl-line")
  (defface evil-insert-state '((t :background  "#005f00")) "The Evil Insert State Hl-line")
  (defface evil-visual-state '((t :background  "#005fff")) "The Evil Visual State Hl-line")
  (defface evil-motion-state '((t :background  "#5f0000")) "The Evil Motion State Hl-line")
  (defface evil-emacs-state '((t :background  "#5f00ff"))  "The Evil Emacs State Hl-line")
  (defface evil-replace-state '((t :background  "#8700ff")) "The Evil Replace State Hl-line")
  (defface evil-hybrid-state '((t :background  "#0087ff")) "The Evil Hybrid State Hl-line")
  (defface evil-evilified-state '((t :background  "#5f5f00")) "The Evil Evilified State Hl-line")
  (defface evil-lisp-state '((t :background  "#875fff")) "The Evil Lisp State Hl-line")
  (defface evil-iedit-state '((t :background  "#8700af")) "The Evil iedit State Hl-line")
  (defface evil-iedit-insert-state '((t :background  "#8700af")) "The Iedit Insert state Hl-line")

  (add-hook 'evil-normal-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-normal-state))))
  (add-hook 'evil-insert-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-insert-state))))
  (add-hook 'evil-visual-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-visual-state))))
  (add-hook 'evil-motion-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-motion-state))))
  (add-hook 'evil-emacs-state-entry-hook    (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-emacs-state))))
  (add-hook 'evil-replace-state-entry-hook  (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-replace-state))))
  (add-hook 'evil-hybrid-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-hybrid-state))))
  (add-hook 'evil-evilified-state-entry-hook (lambda () (interactive) (if (overlayp global-hl-line-overlay)    (overlay-put global-hl-line-overlay 'face 'evil-evilified-state))))
  (add-hook 'evil-lisp-state-entry-hook      (lambda () (interactive) (if (overlayp global-hl-line-overlay)    (overlay-put global-hl-line-overlay 'face 'evil-lisp-state))))
  (add-hook 'evil-iedit-state-entry-hook     (lambda () (interactive) (if (overlayp global-hl-line-overlay)    (overlay-put global-hl-line-overlay 'face 'evil-iedit-state))))
  (add-hook 'evil-iedit-insert-state-entry-hook (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-iedit-insert-state))))

  )

(defun jg_layer/post-init-helm ()
  ;;add in keybinding to kill line in completion window
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-K") 'kill-line)
    )
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
  (global-set-key (kbd "C-c ;") 'expand-abbrev)
  (global-set-key (kbd "C-c >") 'yas-new-snippet)
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
  )


(defun jg_layer/post-init-ibuffer ()
  )

(defun jg_layer/post-init-erlang ()
  ;; (also has a load path set in root el file)
  erlang-root-dir "/usr/local/opt/erlang"
  exec-path (cons "/usr/local/opt/erlang/bin" exec-path)

  )

(defun jg_layer/post-init-python ()
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset nil )
  )

(defun jg_layer/post-init-fci ()
  (add-hook 'change-major-mode-after-body-hook 'fci-mode)
  )

(defun jg_layer/init-rainbow-mode ()
  (use-package rainbow-mode
    :commands (rainbow-mode)
    :config (progn
              (spacemacs/set-leader-keys "t C r" 'rainbow-mode)
              (add-hook 'prog-mode-hook 'rainbow-more))
    )
  )

(defun jg_layer/init-dired-mode ()
  (use-package dired-mode
    :commands (dired-mode)
    :config (add-hook 'dired-mode-hook 'dired-omit-mode))
  )
