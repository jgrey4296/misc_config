;;; emacs/dired/+vars.el -*- lexical-binding: t; -*-

;;-- epa/gpg
(after! epa
  ;; Ascii output of encryptions:
  (setq epa-armor t)
  )

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

;; Omit Patterns
(setq dired-omit-files (rx line-start
                           (or "GPATH"
                               "GRTAGS"
                               "GTAGS"
                               "__init__.py"
                               "__pycache__"
                               ".."
                               (and (*? anychar) ".egg-info")
                               (and "log" ?. (*? anychar))
                               (and "flycheck"
                                    (*? anychar)
                                    ".py")
                               (and ?.
                                    (? (or
                                        "DS_Store"
                                        "ccls-cache"
                                        "class"
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
                                        (and "js" (? ".meta"))
                                        (and "project" (? "ile"))
                                        )
                                       )
                                    )
                               )
                           line-end
                           )
      )

;; image-dired
(setq image-dired-dir (concat doom-cache-dir "image-dired/")
      image-dired-db-file (concat image-dired-dir "db.el")
      image-dired-gallery-dir (concat image-dired-dir "gallery/")
      image-dired-temp-image-file (concat image-dired-dir "temp-image")
      image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
      ;; Screens are larger nowadays, we can afford slightly larger thumbnails
      image-dired-thumb-size 150)
