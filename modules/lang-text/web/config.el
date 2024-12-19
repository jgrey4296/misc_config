;;; lang/web/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")

(defer-load! jg-bindings-total "+bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")

(use-package! web-mode
  :commands web-mode
  :config

  (after! smartparens
    (sp-local-pair 'web-mode "<" ">" :unless '(:add +web-is-auto-close-style-3))

    ;; 1. Remove web-mode auto pairs whose end pair starts with a latter
    ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
    ;;    better.
    ;; 2. Strips out extra closing pairs to prevent redundant characters
    ;;    inserted by smartparens.
    (dolist (alist web-mode-engines-auto-pairs)
      (setcdr alist
              (cl-loop for pair in (cdr alist)
                       unless (string-match-p "^[a-z-]" (cdr pair))
                       collect (cons (car pair)
                                     (string-trim-right (cdr pair)
                                                        "\\(?:>\\|]\\|}\\)+\\'")))))
    (delq! nil web-mode-engines-auto-pairs))

  ;; Use // instead of /* as the default comment delimited in JS
  (setf (alist-get "javascript" web-mode-comment-formats nil nil #'equal) "//")

  (add-hook! 'web-mode-hook #'+web--fix-js-comments-h)
)

(use-package! css-mode
  :commands (css-mode stylus-mode)
  :config
  (add-hook! 'css-mode-hook
             #'hs-minor-mode
             #'smartparens-mode
             #'librarian-insert-minor-mode)

  (setq-hook! 'css-mode-hook
    ;; Correctly continue /* and // comments on newline-and-indent
    comment-line-break-function #'+css/comment-indent-new-line
    ;; Fix `fill-paragraph' not conjoining line comments in CSS modes correctly.
    adaptive-fill-function #'+css-adaptive-fill-fn
    ;; Fix filled lines not being auto-prefixed with a * when needed.
    adaptive-fill-first-line-regexp "\\'[ \t]*\\(?:\\* *\\)?\\'"
    )

  (add-hook! '(css-mode-hook  stylus-mode-hook)
             #'rainbow-mode
             #'tree-sitter!
             )
  )

(use-package! sass-mode
  :commands sass-mode
  :config
  (add-hook! 'sass-mode-hook
             #'rainbow-mode
             )
  )

(use-package! counsel-css
  :when (modulep! :ui ivy)
  :after css-mode
  :hook (css-mode . counsel-css-imenu-setup)
  )

(use-package! emmet-mode
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :commands emmet-mode
  :hook (css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode reason-mode)
  :config
  (when (require 'yasnippet nil t)
    (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  (setq emmet-move-cursor-between-quotes t)
  (setq-hook! 'rjsx-mode-hook emmet-expand-jsx-className? t)
  (map! :map emmet-mode-keymap
        :v [tab] #'emmet-wrap-with-markup
        [tab] #'+web/indent-or-yas-or-emmet-expand
        "M-E" #'emmet-expand-line)
  )

(use-package! jinja2-mode
  :defer t
  )
