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

(rx-let ((filename (*? graph))
         (system (| ".." "TheVolumeSettingsFolder" (: ?. (? filename) "cache")))
         (dotfiles (: ?. (? "_.") (| "CFUserTextEncoding" "DS_Store" "DocumentRevisions-V100" "PKInstallSandboxManager"
                                     "Spotlight-V100" "TemporaryItems" "Trash" "Trashes" "apdisk" "com.apple.timemachine.donotpresent"
                                     "TheVolumeSettingsFolder"
                                     "cups" "debris" "fseventsd"
                                     "offlineimap" "ncftp"
                                     (: "js" (? ".meta"))
                                     )))
         (icons    (| "Icon\015"  (: ?. "thumbnails") ))
         (gtags    (| "GPATH" "GRTAGS" "GTAGS"))
         (build-tools (| "gradlew" "gradlew.bat" (: ?. (| "gradle" "rustup" "doit.db.db" "mono" (: "node" (? "_modules")) (: "npm" (? "-global)"))))))
         (python   (| "__pycache__" (: filename ".egg-info") (: "flycheck" filename ".py") (: ?. (| "ipython" "jupyter" "matplotlib" "mypy.ini" ))))
         (compiled (| (: filename (: ?. "o" "elc" "pyo"))))
         (prolog   (| (: ?. (| "swipl-dir-history" "swp" "swt" ))))
         (java     (| (: filename ".class")))
         (logs     (| (: "log." filename)))
         (configs  (| (: ?. (| (: filename "rc") (: "project" (? "ile")) "venv" ))))
         (latex    (| (: ?. (| "auctex-auto" ))))
         (dropbox  (| (: ?. "dropbox" )))
         (ruby     (| (: ?. "gem")))
         (vcs      (| (: ?. (| (:"git" (? "ignore")) "svn" ))))
         )
  (setq dired-omit-files (rx line-start (| system
                                           dotfiles
                                           icons
                                           gtags
                                           build-tools
                                           python
                                           compiled
                                           prolog
                                           java
                                           logs
                                           configs
                                           latex
                                           dropbox
                                           ruby
                                           vcs
                                           )
                             line-end
                             )
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

;;-- hash check
(setq jg-hash-check-command "shasum %s | sort | guniq -w 40 | awk '{print $2}'"
      jg-hash-check-buffer "*shasum*"
      )
;;-- end hash check

;;-- fold spec
(after! jg-fold-specs
  (setq jg-dired-fold-spec `((dired-mode)
                        :open-all   nil
                        :close-all  nil
                        :toggle     ,(cmd! (dired-hide-subdir 1))
                        :open       nil
                        :open-rec   nil
                        :close      nil))
  (push jg-dired-fold-spec evil-fold-list)
  )
;;-- end fold spec
