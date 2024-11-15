;;; emacs/dired/+vars.el -*- lexical-binding: t; -*-

(defvar! +jg-dired-recursive-switches "-aBhlR --group-directories-first" nil)

(spec-handling-setq! dired 50
                    read-file-name-function #'read-file-name-default
                    )

;;-- dired
(setq-default dired-args '("-ahlD" "-v" "--group-directories-first")
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
              dired-quick-sort-group-directories-last ?y


              )

(defvar jg-dired-du-cmd "du")
(defvar jg-dired-du-args '("-hsc"))
;;-- end dired

;;-- omit-patterns
(defvar jg-dired-omit-file-sources `(,(expand-file-name "~/.gitignore_global")
                                     ,(expand-file-name "tools/ignore/search_ignore" templates-loc)
                                     ))
(defvar jg-dired-omit-extensions '(".a" ".annot" ".aux"
                                   ".bbl" ".beam" ".bin" ".blg" ".bzr/"
                                   ".class" ".cm/" ".cma" ".cmi" ".cmo" ".cmt" ".cmti" ".cmx" ".cmxa" ".cp" ".cps"
                                   ".d64fsl" ".dfsl" ".dx32fsl" ".dx64fsl" ".dxl"
                                   ".elc"
                                   ".fas" ".fasl" ".fmt" ".fn" ".fns" ".fsl" ".fx32fsl" ".fx64fsl"
                                   ".git/" ".glo" ".glob" ".gmo"
                                   ".hg/" ".hi"
                                   ".idx"
                                   ".jam"
                                   ".ky" ".kys"
                                   ".la" ".lbin" ".lib" ".ln" ".lo" ".lof" ".lot" ".lx32fsl" ".lx64fsl"
                                   ".mem" ".mo"
                                   ".o"
                                   ".p64fsl" ".pfsl" ".pg" ".pgs" ".phi" ".pho" ".pyc" ".pyo"
                                   ".so" ".sparcf" ".svn/" ".sx32fsl" ".sx64fsl"
                                   ".tfm" ".toc" ".tp" ".tps" ".ufsl"
                                   ".vee" ".vo" ".vok" ".vos" ".vr" ".vrs"
                                   ".wx32fsl" ".wx64fsl"
                                   ".x86f"
                                   "CVS/" "_MTN/" "_darcs/" "~"
                                    )
                                   )
(rx-let ((filename (*? graph))
         (system (| "." "TheVolumeSettingsFolder" (: ?. (? filename) "cache") "_cache_"))
         (dotfiles (: ?. (? "_.") (| "CFUserTextEncoding" "DS_Store" "DocumentRevisions-V100" "PKInstallSandboxManager"
                                     "Spotlight-V100" "TemporaryItems" "Trash" "Trashes" "apdisk" "com.apple.timemachine.donotpresent"
                                     "TheVolumeSettingsFolder"
                                     "cups" "debris" "fseventsd"
                                     "offlineimap" "ncftp"
                                     (: "js" (? ".meta"))
                                     "doot_defaults.toml"
                                     )))
         (parent "^\\..*$")
         (icons    (| "Icon\015"  (: ?. "thumbnails") ))
         (gtags    (| "GPATH" "GRTAGS" "GTAGS"))
         (build-tools (| "gradlew" "gradlew.bat" (: ?. (| "gradle" "rustup" "doit.db.db" "mono" (: "node" (? "_modules")) (: "npm" (? "-global)"))))))
         (python   (| "__pycache__" (: filename ".egg-info") (: ?. (| "ipython" "jupyter" "matplotlib" "mypy.ini" ))))
         (flycheck "flycheck_")
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
  (defvar jg-dired-omit-files (rx line-start (| system
                                                dotfiles
                                                icons
                                                gtags
                                                build-tools
                                                python
                                                flycheck
                                                compiled
                                                prolog
                                                java
                                                logs
                                                configs
                                                latex
                                                dropbox
                                                ruby
                                                vcs
                                                parent
                                                )
                                  line-end
                                  )
                       )
  (spec-handling-setq! dired-omit-files jg-dired-omit-files)
  (provide 'dired-omit-files-set)
  )

(spec-handling-setq! dired-omit 50
                     dired-omit-files jg-dired-omit-files
                     dired-omit-extensions jg-dired-omit-extensions
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

(defvar jg-hash-check-command "shasum %s | sort | guniq -w 40 | awk '{print $2}'")

(defvar jg-hash-check-buffer "*shasum*")
;;-- end hash check

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
      `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'"               ,(if (eq system-type 'darwin)
                                                                    "open -a Preview -nF"
                                                                  "evince"))
        ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'"               "open")
        ("\\.\\(?:xcf\\)\\'"                                   "open")
        ("\\.csv\\'"                                           "open")
        ("\\.tex\\'"                                           "open")
        ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\)\\(?:\\.part\\)?\\'" "open")
        ("\\.\\(?:rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'"       "open")
        ("\\.\\(?:mp3\\|flac\\)\\'"                            "open")
        ("\\.html?\\'"                                         "open")
        ("\\.md\\'"                                            "open")
        )
      )
;;-- end open cmd

(after! (evil evil-snipe)
  (push 'dired-mode evil-snipe-disabled-modes)
  )

;;-- specs
(spec-handling-add! fold
                    `(dired
                     :modes (dired-mode)
                     :priority 50
                     :triggers (:open-all   nil
                                :close-all  nil
                                :toggle     ,(cmd! (dired-hide-subdir 1))
                                :open       nil
                                :open-rec   nil
                                :close      nil
                                )
                     )
                    )

(spec-handling-add! popup
                    '(dired
                     ("^\\*image-dired" :slot 20 :size 0.8 :select t :quit nil :ttl 0)
                     ("^\\*ranger" :ignore t)
                     ("^\\*CookieCutter\\*" :side bottom :select nil :quit t :ttl 0)
                     ("^\\*file-hashes\\*" :side bottom :select nil :quit t :ttl 3)
                     ("^\\*File Metadata\\*\\'" :width 80 :side left :select nil :quit t :ttl 3)
                     )
                    )
;;-- end specs
