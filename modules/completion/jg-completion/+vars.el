;;; completion/ivy/+vars.el -*- lexical-binding: t; -*-
(defvar jg-completion-file-template-rules (make-hash-table))
(defvar jg-completion-file-templates-dir "/Volumes/documents/github/emacs_files/snippets/file-templates")


(after! (ivy)
  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t
        ivy-rich-parse-remote-buffer nil
        swiper-action-recenter t)
)
(after! (helm helm-files)
  (setq  helm-candidate-number-limit 50
         helm-truncate-lines t
         ;; Remove extraineous helm UI elements
         helm-display-header-line nil
         helm-mode-line-string nil
         helm-ff-auto-update-initial-value nil
         helm-find-files-doc-header nil
         ;; Default helm window sizes
         helm-display-buffer-default-width nil
         helm-display-buffer-default-height 0.25
         ;; When calling `helm-semantic-or-imenu', don't immediately jump to
         ;; symbol at point
         helm-imenu-execute-action-at-once-if-one nil
         ;; disable special behavior for left/right, M-left/right keys.
         helm-ff-lynx-style-map nil
         ;; Matching
         ;; helm-apropos-fuzzy-match     fuzzy
         ;; helm-bookmark-show-location  fuzzy
         ;; helm-buffers-fuzzy-matching  fuzzy
         ;; helm-ff-fuzzy-matching       fuzzy
         ;; helm-file-cache-fuzzy-match  fuzzy
         ;; helm-flx-for-helm-locate     fuzzy
         ;; helm-imenu-fuzzy-match       fuzzy
         ;; helm-lisp-fuzzy-completion   fuzzy
         ;; helm-locate-fuzzy-match      fuzzy
         ;; helm-projectile-fuzzy-match  fuzzy
         ;; helm-recentf-fuzzy-match     fuzzy
         ;; helm-semantic-fuzzy-match    fuzzy
         helm-boring-file-regexp-list (append (list "\\.projects$" "\\.DS_Store$") helm-boring-file-regexp-list)
        ;; helm-boring-buffer-regexp-list
        )
  )

(setq company-idle-delay 1)
(setq-default avy-all-windows t)
