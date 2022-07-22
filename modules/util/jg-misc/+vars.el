;;; util/jg-misc/+vars.el -*- lexical-binding: t; -*-

(setq-default icicle-Completions-text-scale-decrease 0
              indent-tabs-mode nil
              )


;;-- evil-misc
(after! (evil evil-snipe)
  (push 'dired-mode evil-snipe-disabled-modes)
  )
(after! evil-quickscope
  ;; TODO (spacemacs/set-leader-keys "t q" '+jg-personal-toggle-quickscope-always)
  (global-evil-quickscope-always-mode 1)
  )
;;-- end evil-snipe

;;-- shellvars
(setq-default shell-default-shell 'shell
              shell-protect-eshell-prompt 0
              shell-enable-smart-eshell t
              )
;;-- end shellvars

;;-- line_highlighting
(setq-default
              hl-paren-colors '("color-16" "color-16" "color-16" "color-16")
              hl-paren-background-colors '("Springgreen3" "color-26" "color-91" "IndianRed3")
              global-hl-line-modes '(bibtex-mode prog-mode text-mode conf-mode special-mode org-agenda-mode comint-mode)
              )
;;-- end line_highlighting

;;-- undo-limits
(setq-default
              ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
              ;; truncating the undo history and corrupting the tree. See
              ;; https://github.com/syl20bnr/spacemacs/issues/12110
              undo-limit 800000
              undo-strong-limit 12000000
              undo-outer-limit 120000000
              undo-tree-history-directory-alist `(("." . ,(concat doom-cache-dir "undo-tree-hist/")))
              )
;;-- end undo-limits

;;-- personal-vars
(setq-default jg-misc-rps-have-you-played-loc "/Volumes/documents/github/writing/resources/urls/have-you-playeds"
              jg-misc-ivy-predicate-patterns (rx (or "*helpful"
                                                     "*Ibuffer"
                                                     "*helm-"
                                                     "doom"
                                                     "*dired-log"
                                                     "magit"
                                                     "*Free Keys"
                                                     )
                                                 )
              jg-misc-project-cmd-cache-name ".projectile-cmds"
              )
;;-- end personal-vars

;;-- diary
(setq-default diary-file "~/.doom.d/diary")
;;-- end diary

;;-- ligatures
;; TODO move to text
(setq +ligatures-extra-symbols
      '(;; org
        :name          "»"
        :src_block     "»"
        :src_block_end "«"
        :quote         "“"
        :quote_end     "”"
        ;; Functional
        :lambda        "λ"
        ;;:def           "ƒ"
        :composition   "∘"
        :map           "↦"
        ;; Types
        :null          "∅"
        :true          "𝕋"
        :false         "𝔽"
        ;; :int           "ℤ"
        ;; :float         "ℝ"
        ;; :str           "𝕊"
        ;; :bool          "𝔹"
        ;; :list          "𝕃"
        ;; Flow
        :not           "¬"
        :in            "∈"
        :not-in        "∉"
        :and           "∧"
        :or            "∨"
        :for           "∀"
        :some          "∃"
        :return        "⏎"
        :yield         "⤶ "
        ;; Other
        :union         "⋃"
        :intersect     "∩"
        :diff          "∖"
        :tuple         "⨂ "
        :pipe          "|"
        :dot           "•")
      )

;;-- end ligatures

;;-- minibuffer
(defvar +default-minibuffer-maps
  (append '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map
            read-expression-map)
          (cond ((featurep! :completion ivy)
                 '(ivy-minibuffer-map
                   ivy-switch-buffer-map))
                ((featurep! :completion helm)
                 '(helm-map
                   helm-rg-map
                   helm-read-file-map))))
  "A list of all the keymaps used for the minibuffer.")

;;-- end minibuffer

;;-- flycheck
(after! flycheck
  (setq-default flycheck-display-errors-delay 1
                flycheck-display-errors-function nil
                flycheck-help-echo-function nil
                flycheck-process-error-functions nil )
)

;;-- end flycheck

;;-- neotree
(after! neotree
  (push "^__pycache__$" neo-hidden-regexp-list)
  (push "^G\\(PATH\\|R?TAGS\\)$" neo-hidden-regexp-list)
  (push "^__init__.py$" neo-hidden-regexp-list)
  )
;;-- end neotree
