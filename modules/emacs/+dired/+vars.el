;;; emacs/dired/+vars.el -*- lexical-binding: t; -*-

;;-- epa/gpg
(after! epa
  ;; Ascii output of encryptions:
  (setq epa-armor t)
  )
;;-- end epa/gpg

;;-- dired
(setq-default dired-args '("-ahl" "-v" "--group-directories-first")
              insert-directory-program "gls"

              dired-auto-revert-buffer #'dired-buffer-stale-p
              dired-clean-confirm-killing-deleted-buffers nil
              dired-create-destination-dirs 'ask
              dired-dwim-target t
              dired-hide-details-hide-symlink-targets nil
              dired-omit-verbose nil
              dired-recursive-copies  'always
              dired-recursive-deletes 'top
              dired-vc-rename-file t

              +jg-dired-recursive-switches "-aBhlR --group-directories-first"
 )

(after! (dired dired-x dired-quick-sort)
  (setq-default dired-quick-sort-group-directories-last ?y)
  )
;;-- end dired

;;-- omit-patterns

(rx-let ((filename (*? graph))
         (system (| "." "TheVolumeSettingsFolder" (: ?. (? filename) "cache")))
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

;;-- popup
(setq jg-dired-popup-rules
      '(
        ("^\\*image-dired" :slot 20 :size 0.8 :select t :quit nil :ttl 0)
        ("^\\*ranger" :ignore t)
        ))
(after! jg-popup-init
  (+jg-ui-popup-add-rules 'dired jg-dired-popup-rules)
  )
;;-- end popup


;;-- ranger
(setq ranger-cleanup-on-disable t
      ranger-excluded-extensions '("mkv" "iso" "mp4")
      ranger-deer-show-details t
      ranger-max-preview-size 10
      ranger-show-literal nil
      ranger-hide-cursor nil)
;;-- end ranger


;;-- open cmd
(setq dired-guess-shell-alist-user
      `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'"               "open")
        ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'"               "open")
        ("\\.\\(?:xcf\\)\\'"                                   "open")
        ("\\.csv\\'"                                           "open")
        ("\\.tex\\'"                                           "open")
        ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\)\\(?:\\.part\\)?\\'" "open")
        ("\\.\\(?:rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'"       "open")
        ("\\.\\(?:mp3\\|flac\\)\\'"                            "open")
        ("\\.html?\\'"                                         "open")
        ("\\.md\\'"                                            "open"))
      )
;;-- end open cmd