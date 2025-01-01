;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+extra")

(defer-load! jg-bindings-total "+bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")

(advice-add 'elisp-get-var-docstring :around #'+emacs-lisp-append-value-to-eldoc-a)
;; Fixed indenter that intends plists sensibly.
(advice-add 'calculate-lisp-indent :override #'+emacs-lisp--calculate-lisp-indent-a)
;; Recenter window after following definition
(advice-add 'elisp-def :after #'doom-recenter-a)

;; `elisp-mode' is loaded at startup. In order to lazy load its config we need to pretend it isn't loaded

(defer-feature! elisp-mode emacs-lisp-mode)

(use-package! elisp-mode
  :config
  ;; variable-width indentation is superior in elisp. Otherwise, `dtrt-indent'
  ;; and `editorconfig' would force fixed indentation on elisp.
  (add-to-list 'doom-detect-indentation-excluded-modes 'emacs-lisp-mode)

  ;; Enhance elisp syntax highlighting, by highlighting Doom-specific
  ;; constructs, defined symbols, and truncating :pin's in `package!' calls.
  (font-lock-add-keywords
   'emacs-lisp-mode
   (append `(;; custom Doom cookies
             ("^;;;###\\(autodef\\|if\\|package\\)[ \n]" (1 font-lock-warning-face t)))
           ;; Shorten the :pin of `package!' statements to 10 characters
           `(("(package!\\_>" (0 (+emacs-lisp-truncate-pin))))
           ;; highlight defined, special variables & functions
           (when +emacs-lisp-enable-extra-fontification
             `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)))))


  (add-hook! 'emacs-lisp-mode-hook
             #'flycheck-mode
             #'rainbow-delimiters-mode
             #'highlight-quoted-mode
             #'+emacs-lisp-init-straight-maybe-h
             #'abbrev-mode
             #'maybe-ert-test-minor-mode
             )

  (after! librarian
    (add-hook! 'emacs-lisp-mode-hook
               #'librarian-insert-minor-mode
               )
    )

  (setq-hook! 'emacs-lisp-mode-hook
    tab-width 8
    outline-regexp +emacs-lisp-outline-regexp
    outline-level #'+emacs-lisp-outline-level
    evil-surround-pairs-alist (append jg-evil-surround-pairs-base
                                      jg-lisp-surround-pairs)
    flycheck--automatically-enabled-checkers '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package)
    flycheck--automatically-disabled-checkers '()
    )

  )

(use-package! racket-mode
  :commands (racket-mode)
  :config
  (add-hook! 'racket-mode-hook
             #'rainbow-delimiters-mode
             #'highlight-quoted-mode)

    (add-hook 'racket-mode-local-vars-hook #'racket-xp-mode)
    ;; Both flycheck and racket-xp produce error popups, but racket-xp's are
    ;; higher quality so disable flycheck's:
    (add-hook! 'racket-xp-mode-hook
      (defun +racket-disable-flycheck-h ()
        (cl-pushnew 'racket flycheck-disabled-checkers)
        )
      )
    (add-hook 'racket-mode-hook #'racket-smart-open-bracket-mode)

  )

(use-package! buttercup
  :defer t
  :minor ("/.+?-test[/-].+\\.el$" . buttercup-minor-mode)
  :preface
  ;; buttercup.el doesn't define a keymap for `buttercup-minor-mode', as we have
  ;; to fool its internal `define-minor-mode' call into thinking one exists, so
  ;; it will associate it with the mode.
  (defvar buttercup-minor-mode-map (make-sparse-keymap))
  :config
  (when (featurep 'evil)
    (add-hook 'buttercup-minor-mode-hook #'evil-normalize-keymaps))
  )

(use-package! ert
  :config
  (setq ert--selector-history '("t" ":new" ":failed" ":expected"))
  )
