;;; domain-specific/music/+vars.el -*- lexical-binding: t; -*-

(setq-default tidal-interpreter "/usr/local/bin/ghci"
              ;; tidal-interpreter-arguments
              ;; (list "-ghci-script" (expand-file-name "~/github/languageLearning/tidal/.ghci"))
              sclang-auto-scroll-post-buffer t
              sclang-help-path (quote ("/Applications/SuperCollider/Help"))
              sclang-program "sclang"
              sclang-rtf-editor-program "emacs"
              sclang-show-workspace-on-startup nil
              sclang-library-configuration-file (expand-file-name "modules/jg-music-layer/sclang.yaml" doom-user-dir)
              sclang-udp-port 57120
              sclang-eval-line-forward nil
              sclang-runtime-directory (expand-file-name "~/.sclang/")
              sclang-boot-file (expand-file-name "modules/jg-music-layer/startup.scd" doom-user-dir)

              jg-music-tidal-workspace "Tidal Workspace"
              jg-music-sclang-workspace "SCLang Workspace"

              )

(spec-handling-add! lookup-regular
                    (csound-mode
                     ("Csound Manual" . "https://csound.com/docs/manual/PartOverview.html")
                     ("Csound tutorial" . "http://www.csounds.com/toots/index.html")
                     )
                    (sclang-mode
                     ("Supercollider docs" . "http://doc.sccode.org/")
                     )
                    (chuck-mode
                     ("Chuck manual" . "https://chuck.cs.princeton.edu/doc/")
                     )
                    (tidal-mode
                     ("Tidal manual" . "https://tidalcycles.org/docs/")
                     )
                    )

(spec-handling-add! auto-modes
                    '(faust
                      ("\\.dsp\\'" . faustine-mode)
                      )
                    )

(spec-handling-add! company
                    '((faust-mode faustine-mode)
                      (:mode . #'+faust-company-backend))
                    )
