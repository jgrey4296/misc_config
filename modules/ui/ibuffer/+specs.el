;;; +filters.el -*- lexical-binding: t; -*-

(setq-default jg-ibuffer-heuristics (rx (or "backtab"
                                            (regexp "\\.\\.")
                                            (regexp "^[[:alpha:]]\\{2,\\}")
                                            (regexp "which-key")
                                            (regexp "/ S")
                                            )
                                        )
              )

(spec-handling-add! ibuffer-formats
                    '(normal
                     mark modified read-only locked
                     " " (name 18 18 :left :elide)
                     " " (size 10 10 :right)
                     " " (mode 16 16 :left :elide)
                     " " project-relative-file)
                    '(vc-status
                     mark modified read-only locked
                     " " (name 18 18 :left :elide)
                     " " (size 10 10 :right)
                     " " vc-status
                     )
                    '(project
                     mark " " (name 18 18 :left :elide)
                     " " (project-name 10 10 :left)
                     " " project-relative-file
                     )
                    )

;; Filters:

(spec-handling-add! ibuffer-filters
                    '(-clutter (not (or (name . "*http ")
                                        (used-mode . flycheck-error-list-mode)
                                        (used-mode . ivy-mode)
                                        (used-mode . helm-major-mode)
                                        (used-mode . helpful-mode)
                                        (used-mode . special-mode)
                                        (derived-mode . helm-major-mode)
                                        (derived-mode . helpful-mode)
                                        (derived-mode . magit-mode)
                                        (name . "*vc*")
                                        (name . "*shasum*")
                                        (name . "[[:space:]]+\\*.+")
                                        ))
                               )
                    '(dired (used-mode . dired-mode))
                    )

(spec-handling-add! ibuffer-filters
                    '(carousel (name . "->"))
                    '(stars       (name . "^*"))
                    '(music       (or (name . "*\\(tidal\\|SCLang\\)")
                                      (used-mode . sclang-mode)
                                      (used-mode . tidal-mode)
                                      (file-extension . "scd\\|hs\\|tidal")))

                    '(org         (derived-mode . org-mode))
                    '(python      (used-mode . python-mode))
                    '(programming (or (derived-mode . prog-mode)
                                      (mode . ess-mode)
                                      (mode . compilation-mode)))
                    '(text        (and (derived-mode . text-mode) (not (starred-name))))
                    '(TeX         (or (derived-mode . tex-mode)
                                      (mode . latex-mode)
                                      (mode . context-mode)
                                      (mode . ams-tex-mode)))
                    '(bibtex      (used-mode . bibtex-mode))
                    '(web         (or (derived-mode . sgml-mode)
                                      (derived-mode . css-mode)
                                      (mode . javascript-mode)
                                      (mode . js2-mode)
                                      (mode . scss-mode)
                                      (derived-mode . haml-mode)
                                      (mode . sass-mode)))
                            )

(spec-handling-add! ibuffer-filters
                    '(mail (or (mode . message-mode)
                               (mode . mail-mode)
                               (mode . gnus-group-mode)
                               (mode . gnus-summary-mode)
                               (mode . gnus-article-mode))))

(spec-handling-add! ibuffer-filters
                   '(csv     ())
                   '(graphql ())
                   '(json    ())
                   '(logs    ())
                   '(toml    ())
                   '(xml     ())
                   '(yaml    ())
                   '(zip     ())
                    )

(spec-handling-add! ibuffer-filters
                    '(doot ())
                    '(ring-buffers ())
                    )

;; Groups:

(spec-handling-add! ibuffer-groups :form 'override
                    '(default
                       ("*Starred*"   (saved . "stars"))
                       ("Eww Pages"   (name . "\\*eww-"))
                       ("Tomls"       (mode . conf-toml-mode))
                       ("Bibtex"      (mode . bibtex-mode))
                       ("*Project: configs" (projectile-root . "__configs"))
                       ("Dired"       (mode . dired-mode))
                       ;; ("*Project: writing" (projectile-root . "jgrey4296.github.io"))
                       )
                    '(starred ("*Starred*"   (saved . "stars")))
                    '(general
                      ("Starred"     (saved . "stars"))
                      ("org"         (saved . "org"))
                      ("programming" (saved . "programming"))
                      ("dired"       (saved . "dired"))
                      )
                    '(org     ("org"         (saved . "org")))
                    '(dired   ("dired"       (saved . "dired")))
                    )

(spec-handling-add! ibuffer-groups
                    '(projects
                      ("*Starred*" (starred-name))
                      ("*Project: configs*" (projectile-root . "__configs"))
                      ("*Project: modules*" (projectile-root . "modules"))
                      ("*Project: writing*" (projectile-root . "jgrey4296.github.io"))
                      ("*Project: Dropbox*" (projectile-root . "Dropbox"))
                      )
                    '(programming ("programming" (saved . "programming")))
                    )

(spec-handling-add! ibuffer-groups :form 'extend
                       '(programming
                         ("python" (derived-mode . python-mode))
                         ("lisp"   (derived-mode . emacs-lisp-mode))
                         )
                       )
