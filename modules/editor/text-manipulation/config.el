;;; util/text/config.el -*- lexical-binding: t; -*-

(load! "+defs")
(load! "+vars")

(defer-load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")

(after! evil
  (remove-hook! 'after-change-major-mode-hook #'doom--setq-evil-shift-width-for-after-change-major-mode-h)
  )

(add-hook! 'doom-init-ui-hook :append #'+ligature-init-composition-table-h)
(when (memq 'visual-line-mode text-mode-hook)
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'+word-wrap-mode)
  )

(use-package! lint-result-mode
  :config
  (add-hook 'lint-result-mode-hook '+fold/close-all)
  )

(use-package! timeline-mode :defer t)

(use-package! ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :hook (doom-first-buffer . ws-butler-global-mode)
  )

(use-package! smartparens
  :hook (doom-first-buffer . smartparens-global-mode)
  :commands (sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string)
  :config
  (add-to-list 'doom-point-in-string-functions 'sp-point-in-string)
  (add-to-list 'doom-point-in-comment-functions 'sp-point-in-comment)
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  (require 'smartparens-config);; Load default smartparens rules for various languages

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (progn ;; hooks
    (add-hook! 'eval-expression-minibuffer-setup-hook #'doom-init-smartparens-in-eval-expression-h)
    (add-hook! 'minibuffer-setup-hook #'doom-init-smartparens-in-minibuffer-maybe-h)

    ;; Smartparens breaks evil-mode's replace state
    (defvar doom-buffer-smartparens-mode nil)
    (add-hook! 'evil-replace-state-exit-hook #'doom-enable-smartparens-mode-maybe-h)
    (add-hook! 'evil-replace-state-entry-hook #'doom-disable-smartparens-mode-maybe-h)

    ;; Smartparens' navigation feature is neat, but does not justify how
    ;; expensive it is. It's also less useful for evil users. This may need to
    ;; be reactivated for non-evil users though. Needs more testing!
    (setq-hook! 'after-change-major-mode-hook
      sp-navigate-skip-match nil
      sp-navigate-consider-sgml-tags nil)
    )

  ;; Autopair quotes more conservatively; if I'm next to a word/before another
  ;; quote, I don't want to open a new pair or it would unbalance them.
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

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

  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Major-mode specific fixes
  (sp-local-pair 'ruby-mode "{" "}"
                 :pre-handlers '(:rem sp-ruby-pre-handler)
                 :post-handlers '(:rem sp-ruby-post-handler))

  ;; Don't eagerly escape Swift style string interpolation
  (sp-local-pair 'swift-mode "\\(" ")" :when '(sp-in-string-p))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))

  ;; Reasonable default pairs for HTML-style comments
  (sp-local-pair (append sp--html-modes '(markdown-mode gfm-mode))
                 "<!--" "-->"
                 :unless '(sp-point-before-word-p sp-point-before-same-p)
                 :actions '(insert) :post-handlers '(("| " "SPC")))

  ;; Disable electric keys in C modes because it interferes with smartparens
  ;; and custom bindings. We'll do it ourselves (mostly).
  (after! cc-mode
    (setq-default c-electric-flag nil)
    (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
      (define-key c-mode-base-map key nil))

    ;; Smartparens and cc-mode both try to autoclose angle-brackets
    ;; intelligently. The result isn't very intelligent (causes redundant
    ;; characters), so just do it ourselves.
    (define-key! c++-mode-map "<" nil ">" nil)

    (defun +default-cc-sp-point-is-template-p (id action context)
      "Return t if point is in the right place for C++ angle-brackets."
      (and (sp-in-code-p id action context)
           (cond ((eq action 'insert)
                  (sp-point-after-word-p id action context))
                 ((eq action 'autoskip)
                  (/= (char-before) 32)))))

    (defun +default-cc-sp-point-after-include-p (id action context)
      "Return t if point is in an #include."
      (and (sp-in-code-p id action context)
           (save-excursion
             (goto-char (line-beginning-position))
             (looking-at-p "[ 	]*#include[^<]+"))))

    ;; ...and leave it to smartparens
    (sp-local-pair '(c++-mode objc-mode)
                   "<" ">"
                   :when '(+default-cc-sp-point-is-template-p
                           +default-cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC")))

    (sp-local-pair '(c-mode c++-mode objc-mode java-mode)
                   "/*!" "*/"
                   :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  ;; Expand C-style comment blocks.
  (defun +default-open-doc-comments-block (&rest _ignored)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    )
  (sp-local-pair
   '(js2-mode typescript-mode rjsx-mode rust-mode c-mode c++-mode objc-mode
     csharp-mode java-mode php-mode css-mode scss-mode less-css-mode
     stylus-mode scala-mode)
   "/*" "*/"
   :actions '(insert)
   :post-handlers '(("| " "SPC")
                    (" | " "*")
                    ("|[i]\n[i]" "RET")))

  )

(use-package! writegood-mode
  :hook (org-mode markdown-mode rst-mode asciidoc-mode LaTeX-mode)
  )

(use-package! embrace :defer t)

(use-package! dtrt-indent
  ;; Automatic detection of indent settings
  :unless noninteractive
  ;; I'm not using `global-dtrt-indent-mode' because it has hard-coded and rigid
  ;; major mode checks, so I implement it in `doom-detect-indentation-h'.
  :hook ((change-major-mode-after-body read-only-mode) . doom-detect-indentation-h)
  :config
  (defun doom-detect-indentation-h ()
    (unless (or (not after-init-time)
                doom-inhibit-indent-detection
                doom-large-file-p
                (memq major-mode doom-detect-indentation-excluded-modes)
                (member (substring (buffer-name) 0 1) '(" " "*")))
      ;; Don't display messages in the echo area, but still log them
      (let ((inhibit-message (not init-file-debug)))
        (dtrt-indent-mode +1))))

  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t)
  ;; Reduced from the default of 5000 for slightly faster analysis
  (setq dtrt-indent-max-lines 2000)

  ;; always keep tab-width up-to-date
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

  (defvar dtrt-indent-run-after-smie)
  (defadvice! doom--fix-broken-smie-modes-a (fn &optional arg)
    "Some smie modes throw errors when trying to guess their indentation, like
`nim-mode'. This prevents them from leaving Emacs in a broken state."
    :around #'dtrt-indent-mode
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (letf! ((defun symbol-config--guess (beg end)
                (funcall symbol-config--guess beg (min end 10000)))
              (defun smie-config-guess ()
                (condition-case e (funcall smie-config-guess)
                  (error (setq dtrt-indent-run-after-smie t)
                         (message "[WARNING] Indent detection: %s"
                                  (error-message-string e))
                         (message ""))))) ; warn silently
        (funcall fn arg)))))
