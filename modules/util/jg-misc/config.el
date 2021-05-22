
(load! "+funcs")
(load! "+ob-plantuml")
(load! "+vars")
(load! "+popup")
(after! evil
  (load! "+bindings")
)
(after! ivy
  (load! "+ivy_actions")
  )

(after! erlang
  ;; (also has a load path set in root el file)
  (setq erlang-root-dir "/usr/local/opt/erlang"
        exec-path (cons "/usr/local/opt/erlang/bin" exec-path)
        )
  )
(after! epa
  ;; Ascii output of encryptions:
  (setq epa-armor t)
  )
(use-package! free-keys
  :commands (free-keys free-keys-set-prefix)
  :config
  (+jg-misc-free-key-binding-update)
  )
(use-package! undo-tree
  :config
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

(add-hook! doom-first-input
           #'+jg-misc-setup-popup-rules-hook
           #'+jg-misc-binding-hook)

(after! flycheck-plantuml-executable
  (setq flycheck-plantuml-executable (executable-find "plantuml"))
  )

(after! ob-plantuml
  (advice-add #'org-babel-execute:plantuml
              :override #'+jg-misc-ob-plantuml-execute
              '((depth . -100)))
  )
