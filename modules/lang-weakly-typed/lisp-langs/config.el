;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")

(defer-load! jg-bindings-total "+bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")
(add-hook! 'doom-first-file-hook #'+jg-lisp-setup-library-source)

;; `elisp-mode' is loaded at startup. In order to lazy load its config we need to pretend it isn't loaded

(defer-feature! elisp-mode emacs-lisp-mode)

(use-package! elisp-mode
  :interpreter ("doomscript" . emacs-lisp-mode)
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

  ;; UX: Flycheck's two emacs-lisp checkers produce a *lot* of false positives
  ;;   in non-packages (like Emacs configs or elisp scripts), so I disable
  ;;   `emacs-lisp-checkdoc' and set `byte-compile-warnings' to a subset of the
  ;;   original in the flycheck instance (see `+emacs-lisp-linter-warnings').
  (add-hook 'flycheck-mode-hook #'+emacs-lisp-non-package-mode)

  (add-hook! 'emacs-lisp-mode-hook
             ;; Allow folding of outlines in comments
             ;; #'outline-minor-mode
             ;; Make parenthesis depth easier to distinguish at a glance
             #'rainbow-delimiters-mode
             ;; Make quoted symbols easier to distinguish from free variables
             #'highlight-quoted-mode
             ;; Ensure straight sees modifications to installed packages
             #'+emacs-lisp-init-straight-maybe-h
             )

  (after! general-insert
    (add-hook! 'emacs-lisp-mode-hook
               #'general-insert-minor-mode
               )
    )

  (setq-hook! 'emacs-lisp-mode-hook
    ;; Emacs' built-in elisp files use a hybrid tab->space indentation scheme
    ;; with a tab width of 8. Any smaller and the indentation will be
    ;; unreadable. Since Emacs' lisp indenter doesn't respect this variable it's
    ;; safe to ignore this setting otherwise.
    tab-width 8
    ;; Don't treat autoloads or sexp openers as outline headers, we have
    ;; hideshow for that.
    outline-regexp +emacs-lisp-outline-regexp
    outline-level #'+emacs-lisp-outline-level
    evil-surround-pairs-alist (append jg-evil-surround-pairs-base
                                      jg-lisp-surround-pairs)
    )

  )

(use-package! racket-mode
  :commands (racket-mode)
  :config
  (add-hook! 'racket-mode-hook
             #'rainbow-delimiters-mode
             #'highlight-quoted-mode)

  (when (modulep! +xp)
    (add-hook 'racket-mode-local-vars-hook #'racket-xp-mode)
    ;; Both flycheck and racket-xp produce error popups, but racket-xp's are
    ;; higher quality so disable flycheck's:
    (when (modulep! :checkers syntax)
      (add-hook! 'racket-xp-mode-hook
        (defun +racket-disable-flycheck-h ()
          (cl-pushnew 'racket flycheck-disabled-checkers)))))

  (unless (or (modulep! :editor parinfer)
              (modulep! :editor lispy))
    (add-hook 'racket-mode-hook #'racket-smart-open-bracket-mode))

  )

(use-package! ielm
  :defer t
  :config

  ;; Adapted from http://www.modernemacs.com/post/comint-highlighting/ to add
  ;; syntax highlighting to ielm REPLs.
  (setq ielm-font-lock-keywords
        (append '(("\\(^\\*\\*\\*[^*]+\\*\\*\\*\\)\\(.*$\\)"
                   (1 font-lock-comment-face)
                   (2 font-lock-constant-face)))
                (when (require 'highlight-numbers nil t)
                  (highlight-numbers--get-regexp-for-mode 'emacs-lisp-mode))
                (cl-loop for (matcher . match-highlights)
                         in (append lisp-el-font-lock-keywords-2
                                    lisp-cl-font-lock-keywords-2)
                         collect
                         `((lambda (limit)
                             (when ,(if (symbolp matcher)
                                        `(,matcher limit)
                                      `(re-search-forward ,matcher limit t))
                               ;; Only highlight matches after the prompt
                               (> (match-beginning 0) (car comint-last-prompt))
                               ;; Make sure we're not in a comment or string
                               (let ((state (syntax-ppss)))
                                 (not (or (nth 3 state)
                                          (nth 4 state))))))
                           ,@match-highlights))))

  )

(use-package! flycheck-cask
  :commands #'flycheck-cask-setup
  :init
  (add-hook! 'emacs-lisp-mode-hook
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup nil t))

  )

(use-package! flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup)
  )

(use-package! elisp-demos
  ;; adds example code in help buffers
  :defer t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update      :after #'elisp-demos-advice-helpful-update)
  :config
  (advice-add 'elisp-demos--search :around #'+jg-lisp-add-elisp-demos))

(use-package! buttercup
  :defer t
  :minor ("/test[/-].+\\.el$" . buttercup-minor-mode)
  :preface
  ;; buttercup.el doesn't define a keymap for `buttercup-minor-mode', as we have
  ;; to fool its internal `define-minor-mode' call into thinking one exists, so
  ;; it will associate it with the mode.
  (defvar buttercup-minor-mode-map (make-sparse-keymap))
  :config
  (when (featurep 'evil)
    (add-hook 'buttercup-minor-mode-hook #'evil-normalize-keymaps))
  )

(use-package! find-func)

(use-package! elisp-depmap
  ;; https://gitlab.com/mtekman/elisp-depmap.el
  )

(def-project-mode! +emacs-lisp-ert-mode
  :modes '(emacs-lisp-mode)
  :match "/test[/-].+\\.el$"
  :add-hooks '(overseer-enable-mode))

(after! projectile
  (add-to-list 'projectile-project-root-files "info.rkt"))

;;;###package overseer
(autoload 'overseer-test "overseer" nil t)
;; Properly lazy load overseer by not loading it so early:
(remove-hook 'emacs-lisp-mode-hook #'overseer-enable-mode)

(use-package! cl-lib)

(use-package! a)

(use-package! f)

(use-package! s)
