;;; emacs/dired/+vars.el -*- lexical-binding: t; -*-

(defun +jg-dired-var-hook ()
  (setq dgi-commit-message-format "%h %cs %s"
        dgi-auto-hide-details-p nil)

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
        ;; Where to store image caches
        image-dired-dir (concat doom-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150

        dired-omit-verbose nil
        dired-omit-files (rx line-start
                             (or "GPATH"
                                 "GRTAGS"
                                 "GTAGS"
                                 "__init__.py"
                                 "__pycache__"
                                 ".."
                                 (and "log" ?. (*? anychar))
                                 (and "flycheck"
                                      (*? anychar)
                                      ".py")
                                 (and ?.
                                      (? (or "gitignore"
                                             "pylintrc"
                                             "venv"
                                             "DS_Store"
                                             "mypy_cache"
                                             (and "project"
                                                  (? "ile"))
                                             "svn"
                                             "git"
                                             "ccls-cache"
                                             (and "js"
                                                  (? ".meta"))
                                             "elc"
                                             "o"
                                             "pyo"
                                             "swp"
                                             "class"
                                             )
                                         )
                                      )
                                 )
                             line-end
                             )
        )
)
