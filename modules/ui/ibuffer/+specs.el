;;; +filters.el -*- lexical-binding: t; -*-

(setq-default jg-ibuffer-heuristics (rx (or "backtab"
                                            (regexp "\\.\\.")
                                            (regexp "^[[:alpha:]]\\{2,\\}")
                                            (regexp "which-key")
                                            (regexp "/ S")
                                            )
                                        )
              )

(speckler-add! ibuffer-formats
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
                     " " vc-status)
                    '(project
                      mark
                      " " (name 18 18 :left :elide)
                      " " (project-name 10 10 :left)
                      " " project-relative-file)
                    )

;; Agendas
(speckler-add! ibuffer-filters
                    '(agendas (or  (agenda-buffers)
                                   (name . "base_agenda.org")))
                    )

(speckler-add! ibuffer-groups
                    '(agendas ("Agendas" (saved . "agendas")))
                    )

;; Filters:

(speckler-add! ibuffer-filters
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
                                        (name . "*trash-async*")
                                        ))
                               )
                    '(dired (used-mode . dired-mode))
                    )

(speckler-add! ibuffer-filters
                    '(carousel (name . "->"))
                    '(stars       (name . "^*"))
                    '(music       (or (name . "*\\(tidal\\|SCLang\\)")
                                      (used-mode . sclang-mode)
                                      (used-mode . tidal-mode)
                                      (file-extension . "scd\\|hs\\|tidal")))

                    '(org         (and (derived-mode . org-mode)
                                       (not (agenda-buffers))))
                    '(python      (or (derived-mode . python-mode) (derived-mode . python-base-mode)))
                    '(dotnet      (or (derived-mode . csharp-mode)
                                      (derived-mode . csharp-ts-mode)
                                      (derived-mode . shader-mode)
                                      (derived-mode . fsharp-mode)
                                      (derived-mode . csproj-mode)
                                      (derived-mode . sln-mode)
                                      ))
                    '(programming (or (derived-mode . prog-mode)
                                      (mode . ess-mode)
                                      (mode . compilation-mode)))
                    '(tests        (and (derived-mode . prog-mode)
                                        (or (name . "test_")
                                            (name . ".+?[-_]tests?\\."))))
                    '(text        (or (saved . "org")
                                      (mode . markdown-mode)
                                      (saved . "TeX")
                                      (and (derived-mode . text-mode)
                                           (not (starred-name)))))
                    '(TeX         (or (derived-mode . tex-mode)
                                      (mode . latex-mode)
                                      (mode . context-mode)
                                      (mode . ams-tex-mode)))
                    '(web         (or (derived-mode . sgml-mode)
                                      (derived-mode . css-mode)
                                      (mode . javascript-mode)
                                      (mode . js2-mode)
                                      (mode . scss-mode)
                                      (derived-mode . haml-mode)
                                      (mode . sass-mode)))
                            )

(speckler-add! ibuffer-filters
                    '(mail (or (mode . message-mode)
                               (mode . mail-mode)
                               (mode . gnus-group-mode)
                               (mode . gnus-summary-mode)
                               (mode . gnus-article-mode))))

(speckler-add! ibuffer-filters
                   '(csv     (mode . csv-mode))
                   '(graphql (mode . graphql-mode))
                   '(json    (mode . json-mode))
                   '(logs    (or (file-extension . "log") ((basename . "^log\."))))
                   '(toml    (or (mode . conf-toml-mode)
                                 (mode . toml-mode)))
                   '(xml     (mode . xml-mode))
                   '(yaml    (mode . yaml-mode))
                   '(zip     (mode . archive-mode))
                   '(bibtex  (used-mode . bibtex-mode))
                    )

(speckler-add! ibuffer-filters
                    '(ring-buffers ())
                    )

;; Groups:

(speckler-add! ibuffer-groups
                    '(default
                      ("Agendas"     (saved . "agendas"))
                      ("*Processes*" (and (not (derived-mode . shell-mode))
                                          (process)))
                      ("*Shells*"      (or (mode . shell-mode)
                                           (mode . comint-mode)
                                           (name . "^\*repl\*")))
                      ("*Starred*"   (saved . "stars"))
                      ("*Eww Pages*" (name . "\\*eww-"))
                      ("*Project: configs*" (projectile-root . "__configs"))
                      ("Dired"       (mode . dired-mode))
                      ("Mail"        (saved . "mail"))
                      ;; ("*Project: writing" (projectile-root . "jgrey4296.github.io"))
                      ("Lisp"        (mode . emacs-lisp-mode))
                      ("Python"      (or (mode . python-mode) (derived-mode . python-base-mode)))
                      ("DotNet"      (or (derived-mode . csharp-mode)
                                         (derived-mode . csharp-ts-mode)
                                         (derived-mode . shader-mode)
                                         (derived-mode . fsharp-mode)
                                         (derived-mode . csproj-mode)
                                         (derived-mode . sln-mode)))
                      ("Tests"       (saved . "tests"))
                      ("Text"        (saved . "text"))
                      ("Logs"        (or (file-extension . "log")
                                         (basename . "^log\.")))
                      ("Data"        (or (saved . "csv")
                                         (saved . "graphql")
                                         (saved . "json")
                                         (saved . "toml")
                                         (saved . "xml")
                                         (saved . "yaml")
                                         (saved . "zip")
                                         (saved . "bibtex")
                                         (file-extension . "sub")
                                         (file-extension . "names")
                                         (file-extension . "sub_author")
                                         (file-extension . "timeline")
                                         (file-extension . "tags")
                                         (file-extension . "bookmarks")
                                         ))
                      )
                    '(starred ("*Starred*"   (saved . "stars")))
                    '(general
                      ("*Starred*"     (saved . "stars"))
                      ("Org"         (saved . "org"))
                      ("Programming" (saved . "programming"))
                      ("Dired"       (saved . "dired"))
                      )
                    '(text
                      ("Org"         (saved . "org"))
                      ("Markdown"    (mode  . markdown-mode))
                      ("Rst"         (mode  . rst-mode))
                      ("Tex"         (mode  . tex-mode))
                      ("Bibtex"      (mode  . bibtex-mode))
                      )
                    '(dired   ("dired"       (saved . "dired")))
                    )

(speckler-add! ibuffer-groups
                    '(projects
                      ("*Starred*" (starred-name))
                      ("*Project: configs*" (projectile-root . "__configs"))
                      ("*Project: modules*" (projectile-root . "modules"))
                      ("*Project: writing*" (projectile-root . "jgrey4296.github.io"))
                      ("*Project: Dropbox*" (projectile-root . "Dropbox"))
                      )
                    '(programming
                      ("programming" (saved . "programming"))
                      )
                    '(tests
                      ("Tests"       (saved . "tests"))
                      )
                    )

(speckler-add! ibuffer-groups
                       '(programming
                         ("python" (or (derived-mode . python-mode) (derived-mode . python-base-mode)))
                         ("lisp"   (derived-mode . emacs-lisp-mode))
                         )
                       )
