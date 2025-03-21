;;; +spelling.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(advice-add 'ispell-word :around #'+spell-init-ispell-extra-args-a)
(advice-add 'flyspell-auto-correct-word :around #'+spell-init-ispell-extra-args-a)
(advice-add 'spell-fu--faces-at-point :around #'+spell--fix-face-detection-a)
(advice-add 'spell-fu--word-add-or-remove :before #'+spell--create-word-dict-a)

(use-package! flyspell ; built-in
  :defer t
  :preface
  ;; `flyspell' is loaded at startup. In order to lazy load its config we need
  ;; to pretend it isn't loaded.
  (defer-feature! flyspell flyspell-mode flyspell-prog-mode)
  :config
  (provide 'ispell)
  (add-hook! 'flyspell-mode-hook #'+spell-inhibit-duplicate-detection-maybe-h)
  )

(use-package! ispell
  :preface
  (defer-feature! ispell ispell-minor-mode)
  :config
  (add-hook 'text-mode-hook #'+spell-remove-run-together-switch-for-aspell-h)

  (setq ispell-aspell-dict-dir     (ispell-get-aspell-config-value "dict-dir")
        ispell-aspell-data-dir     (ispell-get-aspell-config-value  "data-dir")
        )
  )

(use-package! spell-fu
  :defer t
  :when (executable-find "aspell")
  :init
  :config
  (add-hook 'spell-fu-mode-hook #'+spell-init-excluded-faces-h)
  )

(use-package! flyspell-correct
  :commands flyspell-correct-previous
  :after flyspell
  :config
  (require 'flyspell-correct-ivy nil t)
  )

(use-package! flyspell-lazy
  :after flyspell
  :config
  ;; Fix #3357: flyspell-lazy inhibits flyspell entirely in message-mode
  ;; derivatives (e.g. for notmuch users).
  (setq-hook! 'message-mode-hook flyspell-lazy-disallow-buffers nil)
  )

(use-package! company-ispell
  :when (modulep! :ide company)
)

(use-package! wordnut
  :defer t
  :init
  (add-hook 'wordnut-mode-hook 'outline-minor-mode)
  )

(use-package! osx-dictionary
  :when (eq system-type 'darwin)
  :defer t
  )

(use-package! synosaurus
  :defer t
  :config
  (setq synosaurus-choose-method nil)
  )

(use-package! helm-wordnet
  :when (modulep! :ui helm)
  :defer t
  )

(use-package! define-word :defer t)

;;-- defs

(defvar +spell-excluded-faces-alist nil)

(defvar +spell-correct-interface #'+spell-correct-ivy-fn)

(defvar +spell-excluded-faces-alist
  '((markdown-mode
     . (markdown-code-face markdown-html-attr-name-face
                           markdown-html-attr-value-face markdown-html-tag-name-face
                           markdown-inline-code-face markdown-link-face markdown-markup-face
                           markdown-plain-url-face markdown-reference-face markdown-url-face))
    (org-mode
     . (org-block org-block-begin-line org-block-end-line org-cite org-cite-key
                  org-code org-date org-footnote org-formula org-inline-src-block
                  org-latex-and-related org-link org-meta-line org-property-value
                  org-ref-cite-face org-special-keyword org-tag org-todo
                  org-todo-keyword-done org-todo-keyword-habt org-todo-keyword-kill
                  org-todo-keyword-outd org-todo-keyword-todo org-todo-keyword-wait
                  org-verbatim))
    (latex-mode
     . (font-latex-math-face font-latex-sedate-face font-lock-function-name-face
                             font-lock-keyword-face font-lock-variable-name-face)))
  "Faces in certain major modes that spell-fu will not spellcheck.")

;;-- end defs

;;-- spelling
(setq ispell-program-name (executable-find "aspell")
      ispell-extra-args '("--sug-mode=ultra" "--run-together")
      ispell-personal-dictionary (expand-file-name "spelling/aspell.en.pws" templates-loc)
      ispell-current-personal-dictionary ispell-personal-dictionary
      spell-fu-directory (concat doom-data-dir "spell-fu")
      flyspell-popup-correct-delay 0.8

      flyspell-lazy-idle-seconds 1
      flyspell-lazy-window-idle-seconds 3

      flyspell-issue-welcome-flag nil
      ;; Significantly speeds up flyspell, which would otherwise print
      ;; messages for every word when checking the entire buffer
      flyspell-issue-message-flag nil
      )

;;-- end spelling

(speckler-new-hook! flyspell-predicate (key val)
  "Set local flyspec checkers"
  (setq-local flyspell-generic-check-word-predicate (upfun! val))
  )

(speckler-add! flyspell-predicate ()
  `(markdown-mode #'+markdown-flyspell-word-p)
  `(gfm-mode      #'+markdown-flyspell-word-p)
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    March 21, 2025
;; Modified:   March 21, 2025
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
;;; +spelling.el ends here
