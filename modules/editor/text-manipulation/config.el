;;; util/text/config.el -*- lexical-binding: t; -*-
(load! "+defs")
(load! "+vars")
(after! jg-bindings-total
  (load! "evil/+operators")
  (load! "evil/+motions")
  (load! "evil/+state")
  (load! "evil/+text-obj")
  (load! "+bindings")
  (load! "+advice")
)
(load! "modes/+derived-modes")

(use-package! academic-phrases :defer t)

(use-package! highlight-parentheses :defer t)

(use-package! helm-wordnet :defer t)

(use-package! evil-string-inflection
  :defer t
  :commands evil-operator-string-inflection
  )

(use-package! lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs lorem-ipsum-insert-list)
)

(use-package! rainbow-mode
  :defer t
  :init
  (add-hook! 'prog-mode-hook 'rainbow-mode)
)

(use-package! evil-string-inflection
  :defer t
  :commands evil-operator-string-inflection
  )

(use-package! license-templates :defer)

(use-package! lint-result-mode
  :config
  (add-hook 'lint-result-mode-hook '+fold/close-all)
  )

(use-package! vundo
  :commands vundo
  )

(use-package! undo-fu
  :defer t
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq-default
              ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
              ;; truncating the undo history and corrupting the tree. See
              ;; https://github.com/syl20bnr/spacemacs/issues/12110
              undo-limit 800000
              undo-strong-limit 12000000
              undo-outer-limit 120000000
              )
  )

(use-package! highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character)
  :config
  ;; HACK: If this package is loaded too early (by the user, and in terminal
  ;;   Emacs), then `highlight-indent-guides-auto-set-faces' will have been
  ;;   called much too early to set its faces correctly. To get around this, we
  ;;   need to call it again, but at a time when I can ensure a frame exists an
  ;;   the current theme is loaded.
  (when (doom-context-p 'init)
    (add-hook 'doom-first-buffer-hook #'highlight-indent-guides-auto-set-faces))

  ;; `highlight-indent-guides' breaks when `org-indent-mode' is active
  (add-hook! 'org-mode-local-vars-hook

(defun +indent-guides-disable-maybe-h ()
      (and highlight-indent-guides-mode
           (bound-and-true-p org-indent-mode)
           (highlight-indent-guides-mode -1)))))

(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold)))

(defadvice! +hl-todo-clamp-font-lock-fontify-region-a (fn &rest args)
    "Fix an `args-out-of-range' error in some modes."
    :around #'hl-todo-mode
    (letf!

(defun font-lock-fontify-region (beg end &optional loudly)
             (funcall font-lock-fontify-region (max beg 1) end loudly))
      (apply fn args)))

  ;; Use a more primitive todo-keyword detection method in major modes that
  ;; don't use/have a valid syntax table entry for comments.
  (add-hook! '(pug-mode-hook haml-mode-hook)

(defun +hl-todo--use-face-detection-h ()
      "Use a different, more primitive method of locating todo keywords."
      (set (make-local-variable 'hl-todo-keywords)
           '(((lambda (limit)
                (let (case-fold-search)
                  (and (re-search-forward hl-todo-regexp limit t)
                       (memq 'font-lock-comment-face (ensure-list (get-text-property (point) 'face))))))
              (1 (hl-todo-get-face) t t))))
      (when hl-todo-mode
        (hl-todo-mode -1)
        (hl-todo-mode +1)))))

(use-package! evil-iedit-state
  :defer t
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil)
  :config

(define-advice iedit-show-all (:override ()
                                 +jg-misc-iedit-show-all)
    " Override iedit's show all so it doesn't mess with invisible line movement"
    (remove-from-invisibility-spec '(iedit-invisible-overlay-name . t))
    (remove-overlays nil nil iedit-invisible-overlay-name t)
  )

)

(use-package! timeline-mode :defer t)

(when (memq 'visual-line-mode text-mode-hook)
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'+word-wrap-mode))

(add-hook! 'doom-init-ui-hook :append (defun +ligature-init-composition-table-h ()
                                        (dolist (char-regexp +ligatures-composition-alist)
                                          (set-char-table-range
                                           +ligature--composition-table
                                           (car char-regexp) `([,(cdr char-regexp) 0 font-shape-gstring])))
                                        (set-char-table-parent +ligature--composition-table composition-function-table))
           )

(spec-handling-new-hooks! rotate-text
                          (setq-local rotate-text-local-symbols    (plist-get val :symbols)
                                      rotate-text-local-words      (plist-get val :words)
                                      rotate-text-local-patterns   (plist-get val :patterns)
                                      )
                          )

(spec-handling-new-hooks! whitespace-cleanup
                          (setq-local jg-text-whitespace-clean-hook (ensure-list val))
                          )

(spec-handling-new-hooks! ligatures
                          (setq-local prettify-symbols-alist
                                      (let (head alist)
                                        (while val
                                          (setq head (pop val))
                                          (pcase (pop val)
                                            ((and c (guard (characterp c)))
                                             (push (cons head c) alist))
                                            ((and c (guard (keywordp c)) (let l (plist-get +ligatures-extra-symbols c)) (guard l))
                                             (push (cons head l) alist))
                                            )
                                          )
                                        alist
                                        )
                                      )
                          )
