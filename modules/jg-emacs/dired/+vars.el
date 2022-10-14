;;; emacs/dired/+vars.el -*- lexical-binding: t; -*-

;;-- epa/gpg
(after! epa
  ;; Ascii output of encryptions:
  (setq epa-armor t)
  )
;;-- end epa/gpg

;;-- dired-misc
(setq dired-create-destination-dirs 'ask
      dired-vc-rename-file t
      dired-clean-confirm-killing-deleted-buffers nil
      dired-hide-details-hide-symlink-targets nil
      ;; don't prompt to revert; just do it
      dired-auto-revert-buffer t
      ;; suggest a target for moving/copying intelligently
      dired-dwim-target t
      ;; Always copy/delete recursively
      dired-recursive-copies  'always
      dired-recursive-deletes 'top
      dired-omit-verbose nil

      +jg-dired-recursive-switches "-aBhlR --group-directories-first"
      )

(after! (dired dired-x dired-quick-sort)
  (setq dired-quick-sort-group-directories-last ?y)
  )
;;-- end dired-misc

;;-- omit-patterns
(setq dired-omit-files (rx line-start
                           (or "GPATH"
                               "GRTAGS"
                               "GTAGS"
                               "__init__.py"
                               "__pycache__"
                               "Icon\015"
                               ".."
                               "TheVolumeSettingsFolder"
                               (and (*? anychar) ".egg-info")
                               (and (*? anychar) ".elc")
                               (and "log" ?. (*? anychar))
                               (and "flycheck"
                                    (*? anychar)
                                    ".py")
                               (and ?.
                                    (? (or
                                        "DS_Store"
                                        "ccls-cache"
                                        "class"
                                        "debris"
                                        "elc"
                                        "git"
                                        "gitignore"
                                        "mypy.ini"
                                        "mypy_cache"
                                        "o"
                                        "pylintrc"
                                        "pyo"
                                        "pytest_cache"
                                        "svn"
                                        "swp"
                                        "node_modules"
                                        "venv"
                                        "auctex-auto"
                                        (and "js" (? ".meta"))
                                        (and "project" (? "ile"))
                                        "PKInstallSandboxManager"
                                        "TemporaryItems"
                                        "Trashes"
                                        "fseventsd"
                                        "DocumentRevisions-V100"
                                        "Spotlight-V100"
                                        "apdisk"
                                        "Trash"
                                        "cache"
                                        "cups"
                                        "gem"
                                        "ipython"
                                        "jupyter"
                                        "matplotlib"
                                        "mono"
                                        "npm"
                                        "ncftp"
                                        "offlineimap"
                                        "rustup"
                                        "swt"
                                        "thumbnails"
                                        "npm-global"
                                        "swipl-dir-history"
                                        "CFUserTextEncoding"
                                        "dropbox.cache"
                                        "dropbox"
                                        "com.apple.timemachine.donotpresent"
                                        "_.TemporaryItems"
                                        "_.Trashes"
                                        "_.apdisk"
                                        "_.com.apple.timemachine.donotpresent"
                                        "_Machinarium"
                                        "_PowerLineUtility_Win_180816.zip"
                                        "_TheVolumeSettingsFolder"
                                        )
                                       )
                                    )
                               )
                           line-end
                           )
      )
;;-- end omit-patterns

;;-- image-dired
(setq image-dired-dir (concat doom-cache-dir "image-dired/")
      image-dired-db-file (concat image-dired-dir "db.el")
      image-dired-gallery-dir (concat image-dired-dir "gallery/")
      image-dired-temp-image-file (concat image-dired-dir "temp-image")
      image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
      ;; Screens are larger nowadays, we can afford slightly larger thumbnails
      image-dired-thumb-size 150)
;;-- end image-dired

;;-- dgi
(setq dgi-commit-message-format "%h %cs %s"
      dgi-auto-hide-details-p nil)
;;-- end dgi
