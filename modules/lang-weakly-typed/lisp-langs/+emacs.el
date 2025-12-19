;;; +emacs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

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
             #'hs-minor-mode
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
    flycheck--automatically-enabled-checkers '()
    flycheck--automatically-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package)
    )

  (add-hook 'emacs-lisp-mode-hook
            (defun jg-lisp-disable-checkers ()
              (push 'emacs-lisp-checkdoc flycheck--automatically-disabled-checkers)
              (push 'emacs-lisp-package flycheck--automatically-disabled-checkers)
              ))

  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 27, 2025
;; Modified:   July 27, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; +emacs.el ends here
