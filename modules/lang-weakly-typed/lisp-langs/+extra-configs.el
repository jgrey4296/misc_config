;;; +extra-configs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

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

(use-package! elisp-demos
  ;; adds example code in help buffers
  :defer t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update      :after #'elisp-demos-advice-helpful-update)
  :config
  (advice-add 'elisp-demos--search :around #'+jg-lisp-add-elisp-demos))

(use-package! find-func)

(use-package! elisp-depmap
  ;; https://gitlab.com/mtekman/elisp-depmap.el
  )

(use-package! overseer
  :defer t
  :init
  (def-project-mode! +emacs-lisp-ert-mode
    :modes '(emacs-lisp-mode)
    :match "/test[/-].+\\.el$"
    :add-hooks '(overseer-enable-mode)
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 10, 2024
;; Modified:   September 10, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +extra-configs.el ends here
