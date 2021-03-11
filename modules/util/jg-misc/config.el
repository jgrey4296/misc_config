 (setq-default shell-default-shell 'shell
               shell-protect-eshell-prompt 0
               shell-enable-smart-eshell t
               )

(after! erlang
  ;; (also has a load path set in root el file)
  (setq erlang-root-dir "/usr/local/opt/erlang"
        exec-path (cons "/usr/local/opt/erlang/bin" exec-path)
        )
  )
(use-package! rainbow-mode
  :defer t
  :init
  (map! :leader
        :prefix "t v"
        "r" 'rainbow-mode)
  (add-hook 'prog-mode-hook 'rainbow-mode)
)
(use-package! evil-string-inflection
  :defer t
  :commands evil-operator-string-inflection
  :init
  (map! :n "g '" 'evil-operator-string-inflection)
  )
(use-package! free-keys
  :defer t
  :commands (free-keys free-keys-set-prefix)
  :init
  (map! :leader
        (:prefix ("a U" . "Utilities")
         (:prefix ("f" . "free-keys")
          "k" 'free-keys
          "p" 'free-keys-set-prefix
          )
         )
        )
  )
(use-package! highlight-parentheses
  :defer t
  :init
  (setq hl-paren-colors '("color-16" "color-16" "color-16" "color-16")
        hl-paren-background-colors '("Springgreen3" "color-26" "color-91" "IndianRed3"))
  )
(use-package! undo-tree
  :init
  (map! :leader
        "b u" '+jg-misc-undo-tree
    )
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
        ;; truncating the undo history and corrupting the tree. See
        ;; https://github.com/syl20bnr/spacemacs/issues/12110
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000
        undo-tree-history-directory-alist
        `(("." . ,(concat doom-cache-dir "undo-tree-hist/"))))

  ;; Compress undo-tree history files with zstd, if available. File size isn't
  ;; the (only) concern here: the file IO barrier is slow for Emacs to cross;
  ;; reading a tiny file and piping it in-memory through zstd is *slightly*
  ;; faster than Emacs reading the entire undo-tree file from the get go (on
  ;; SSDs). Whether or not that's true in practice, we still enjoy zstd's ~80%
  ;; file savings (these files add up over time and zstd is so incredibly fast).
  (when (executable-find "zstd")
    (defadvice! doom--undo-tree-make-history-save-file-name-a (file)
      :filter-return #'undo-tree-make-history-save-file-name
      (concat file ".zst")))

  ;; Strip text properties from undo-tree data to stave off bloat. File size
  ;; isn't the concern here; undo cache files bloat easily, which can cause
  ;; freezing, crashes, GC-induced stuttering or delays when opening files.
  (defadvice! doom--undo-tree-strip-text-properties-a (&rest _)
    :before #'undo-list-transfer-to-tree
    (dolist (item buffer-undo-list)
      (and (consp item)
           (stringp (car item))
           (setcar item (substring-no-properties (car item))))))

  ;; Undo-tree is too chatty about saving its history files. This doesn't
  ;; totally suppress it logging to *Messages*, it only stops it from appearing
  ;; in the echo-area.
  (advice-add #'undo-tree-save-history :around #'doom-shut-up-a)

  )

(load! "+funcs")
(after! evil
  (load! "+bindings")
  )
