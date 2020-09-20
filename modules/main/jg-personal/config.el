;; jg_emacs config.el
;; loaded fourth

(message "Configuring Personal Preferences")
(load! "+variables")
(load! "+bindings")
(load! "+funcs")
(load! "+evil-ex-setup")
(load! "+helm-funcs")
(load! "+ibuffer-funcs")
(load! "+python-funcs")
(load! "+dired-funcs")


(use-package! hl-line
  :init
  (global-hl-line-mode)
  )
(use-package! hi-lock
  :init
  (global-hi-lock-mode)
  :config
  (setq hi-lock-auto-select-face t)
  )

(after! (evil evil-snipe)
  (push 'dired-mode evil-snipe-disabled-modes)

  )
(after! yasnippet
  ;; If an error occurs, change yas-installed-snippets-dir is not in yas-snippet-dirs
  ;; as it is obsolete
  (setq yas-snippet-dirs `(,(expand-file-name "~/.doom.d/snippets/")))
  ;; ,(expand-file-name "~/github/otherLibs/yasnippet-snippets/snippets")
  ;; ,(expand-file-name "~/github/otherLibs/yasnippet-snippets")
  )
(after! evil-quickscope
  ;; TODO (spacemacs/set-leader-keys "t q" '+jg-personal-toggle-quickscope-always)
  (global-evil-quickscope-always-mode 1)
  )
(after! (evil hl-line)
  (message "Adjusting up hl-line")
  ;; Set up faces for hl-line colour sync to status
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

  ;; hooks for evil state entry hooks to change hl-line colour
  (add-hook 'evil-normal-state-entry-hook       (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-normal-state))))
  (add-hook 'evil-insert-state-entry-hook       (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-insert-state))))
  (add-hook 'evil-visual-state-entry-hook       (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-visual-state))))
  (add-hook 'evil-motion-state-entry-hook       (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-motion-state))))
  (add-hook 'evil-emacs-state-entry-hook        (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-emacs-state))))
  (add-hook 'evil-replace-state-entry-hook      (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-replace-state))))
  (add-hook 'evil-hybrid-state-entry-hook       (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-hybrid-state))))
  (add-hook 'evil-evilified-state-entry-hook    (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-evilified-state))))
  (add-hook 'evil-lisp-state-entry-hook         (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-lisp-state))))
  (add-hook 'evil-iedit-state-entry-hook        (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-iedit-state))))
  (add-hook 'evil-iedit-insert-state-entry-hook (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-iedit-insert-state))))
  )
(after! (dired dired-quick-sort)
  (setq dired-quick-sort-group-directories-last ?y)
  )
(after! ibuffer
  (add-transient-hook! 'ibuffer-hook '+jg-personal-setup-ibuffer)
  )
(after! python
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset nil
                python-shell-interpreter-args "-i"
                python-shell-interpreter "python"
                python-shell-completion-native-enable t
                python-shell-virtualenv-root "~/anaconda3/envs"
                python-shell--interpreter nil
                python-shell--interpreter-args nil
                )
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)
  (map! :mode python-mode
        :leader
        :prefix "i"
        "d" '+jg-personal-python-toggle-breakpoint
        )
  (map! :map 'python-mode-map
        :n "z d" '+jg-personal-toggle-all-defs
        :n "z C" '+jg-personal-close-class-defs
        )
)
(after! neotree
  (push "^__pycache__$" neo-hidden-regexp-list)
  (push "^G\\(PATH\\|R?TAGS\\)$" neo-hidden-regexp-list)
  (push "^__init__.py$" neo-hidden-regexp-list)
  (map! :leader
        :n "f t" 'neotree-toggle
        )
  )
(after! helm-gtags
  ;; Adapated from helm-gtags spacemacs layer
  ;; (let ((jumpl (intern (format "spacemacs-jump-handlers-%S" 'python-mode))))
  ;;   (when (boundp jumpl)
  ;;     (add-to-list jumpl 'spacemacs/helm-gtags-maybe-dwim 'append)))

  (map! :mode 'python-mode
        :prefix "j"
        "C" 'helm-gtags-create-tags
        "d" 'helm-gtags-find-tag
        "D" 'helm-gtags-find-tag-other-window
        "G" 'helm-gtags-dwim-other-window
        "i" 'helm-gtags-tags-in-this-function
        "l" 'helm-gtags-parse-file
        "n" 'helm-gtags-next-history
        "p" 'helm-gtags-previous-history
        "r" 'helm-gtags-find-rtag
        "R" 'helm-gtags-resume
        "s" 'helm-gtags-select
        "S" 'helm-gtags-show-stack
        "y" 'helm-gtags-find-symbol
        "U" 'helm-gtags-update-tags
        )
  )
(after! (dired pysenv)
    """ Remove the annoying python-shell-setup advice """
    (add-transient-hook! 'dired-mode
      (map! :map dired-mode-map
        :localleader
        :n "v" 'pyvenv-activate
        )
      )
    )
(after! (origami python-origami)
  (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
  (add-to-list 'origami-parser-alist '(python-mode . +jg-origami-python-parser))
  )
(after! vlf
    (map! :mode vlf-mode
         :n "] A" 'vlf-next-batch-from-point
         :n "] a" 'vlf-next-batch
         :n "[ a" 'vlf-prev-batch
    ;; TODO
    ;; (spacemacs/set-leader-keys "a U v " 'vlf-set-batch-size))
    )
  )
(after! (featurep! :completion helm)
    (setq! helm-find-files-actions
          (append `(,(car helm-find-files-actions))
                  '(("Open Random" . +jg-personal-helm-open-random-action))
                  '(("Describe Random" . +jg-personal-helm-describe-random-action))
                  '(("Open Random External" . +jg-personal-helm-open-random-external-action))
                  (cdr helm-find-files-actions))
          )
    )
