;;; util/text/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+spelling")
(local-load! "+formatting")
(local-load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")

(add-hook! 'doom-init-ui-hook :append #'+ligature-init-composition-table-h)

(when (memq 'visual-line-mode text-mode-hook)
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'+word-wrap-mode)
  )

(use-package! smartparens
  :commands (sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string)
  :config
  (smartparens-global-mode -1)
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  ;; Autopair quotes more conservatively; if I'm next to a word/before another
  ;; quote, I don't want to open a new pair or it would unbalance them.
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list)
    )

  ;; Expand {|} => { | }
  ;; Expand {|} => {
  ;;   |
  ;; }
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; Don't autopair opening braces if before a word character or
             ;; other opening brace. The rationale: it interferes with manual
             ;; balancing of braces, and is odd form to have s-exps with no
             ;; whitespace in between, e.g. ()()(). Insert whitespace if
             ;; genuinely want to start a new form in the middle of a word.
             :unless '(sp-point-before-word-p sp-point-before-same-p)))

  ;; Disable electric keys in C modes because it interferes with smartparens
  ;; and custom bindings. We'll do it ourselves (mostly).
  (after! cc-mode
    (+jg-text-setup-smartparens--cc-mode)
    )

  (sp-local-pair '(js2-mode typescript-mode rjsx-mode rust-mode c-mode c++-mode objc-mode
                   csharp-mode java-mode php-mode css-mode scss-mode less-css-mode
                   stylus-mode scala-mode)
                 "/*" "*/"
                 :actions '(insert)
                 :post-handlers '(("| " "SPC")
                                  (" | " "*")
                                  ("|[i]\n[i]" "RET"))
                 )
  )

(use-package! embrace :defer t)

(use-package! mapspace-state :after evil)

(use-package! spechar-state :after evil)

(add-hook 'jg-ui-transient-toggles-hook #'+jg-ui-build-transient-format)
