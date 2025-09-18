;;; +filters.el -*- lexical-binding: t; -*-

(speckler-new! ibuffer-filters (key val)
  "Register ibuffer filters"
  :target ibuffer-saved-filters
  :loop 'collect
  (cons (symbol-name key) val)
  )

(speckler-new! ibuffer-groups (key val)
  "Register ibuffer groups"
  :target ibuffer-saved-filter-groups
  :loop 'collect
  (cons (symbol-name key) val)
  )

(speckler-new! ibuffer-formats (key val)
  "Register ibuffer formats"
  :target ibuffer-formats
  :loop 'collect
  val
  )

(speckler-add! ibuffer-formats ()
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

;;-- agenda
(speckler-add! ibuffer-filters ()
  '(agendas (or  (agenda-buffers)
                 (name . "base_agenda.org")))
  )

(speckler-add! ibuffer-groups ()
  '(agendas ("Agendas" (saved . "agendas")))
  )

;;-- end agenda

;; Filters:


(speckler-add! ibuffer-filters () ;; special
  :override t
  '(stars       (and (name . "\\`\\*\\w")
                     (not (saved . "eww"))))
  '(space-star  (or (name . "\\`\\*\\W")
                    (name . "\\`\s\\*")))
  '(eww         (name . "\\*eww-"))
  '(addfiles    (name . "^\+"))
  '(carousel    (name . "->"))
  '(music       (or (name . "*\\(tidal\\|SCLang\\)")
                    (used-mode . sclang-mode)
                    (used-mode . tidal-mode)
                    (file-extension . "scd\\|hs\\|tidal")))
  '(dired       (used-mode . dired-mode))
  '(help        (or (derived-mode . helpful-mode)
                    (derived-mode . info-mode)
                    (derived-mode . help-mode)
                    ))
  '(-clutter (not (or
                   (used-mode . flycheck-error-list-mode)
                   (used-mode . ivy-mode)
                   (used-mode . helm-major-mode)
                   (used-mode . special-mode)
                   (derived-mode . helm-major-mode)
                   (derived-mode . magit-mode)
                   ))
    )
  )

(speckler-add! ibuffer-filters () ;; programming
  :override t
  '(python      (or (derived-mode . python-mode)
                    (derived-mode . python-base-mode)
                    (file-extension . "\\.py'")
                    ))
  '(godot       (or (derived-mode . gdscript-mode)
                    (derived-mode . gdscript-ts-mode)
                    (file-extension . "\\.gd'")
                    ))
  '(dotnet      (or (derived-mode . csharp-mode)
                    (derived-mode . csharp-ts-mode)
                    (derived-mode . shader-mode)
                    (derived-mode . fsharp-mode)
                    (derived-mode . csproj-mode)
                    (derived-mode . sln-mode)
                    ))
  '(kotlin      (or (derived-mode . kotlin-mode)
                    (derived-mode . kotlin-ts-mode)))
  '(rust        (derived-mode . rustic-mode))
  '(elixir      (derived-mode . elixir-mode))
  '(lisp        (and (or (derived-mode . emacs-lisp-mode)
                         (file-extension . "\\.el"))
                     (not (saved . "stars"))))
  '(programming (or (derived-mode . prog-mode)
                    (mode . ess-mode)
                    (mode . compilation-mode)))
  '(tests        (and (derived-mode . prog-mode)
                      (or (name . "test_")
                          (name . ".+?[-_]tests?\\."))))

  '(web         (or (derived-mode . sgml-mode)
                    (derived-mode . css-mode)
                    (mode . javascript-mode)
                    (mode . js2-mode)
                    (mode . scss-mode)
                    (derived-mode . haml-mode)
                    (mode . sass-mode)))
  )

(speckler-add! ibuffer-filters () ;; text
  :override t
  '(text        (or (saved . "org")
                    (mode . markdown-mode)
                    (saved . "TeX")
                    (and (derived-mode . text-mode)
                         (not (starred-name)))))
  '(org         (and (derived-mode . org-mode)
                     (not (agenda-buffers))))
  '(TeX         (or (derived-mode . tex-mode)
                    (mode . latex-mode)
                    (mode . context-mode)
                    (mode . ams-tex-mode)))
  )

(speckler-add! ibuffer-filters () ;; mail
  '(mail (or (mode . message-mode)
             (mode . mail-mode)
             (mode . gnus-group-mode)
             (mode . gnus-summary-mode)
             (mode . gnus-article-mode))))

(speckler-add! ibuffer-filters () ;; data
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

;; Groups:

(speckler-add! ibuffer-groups () ;; default
  :override t
  '(default
    ("*Processes*" (and (not (derived-mode . shell-mode))
                        (process)))
    ("*Shells*"      (or (mode . shell-mode)
                         (mode . comint-mode)
                         (name . "^\*repl\*")))
    ("*Starred*"   (and (saved . "stars")
                        (not (derived-mode . shell-mode))
                        (not (saved . "space-star"))
                        (not (saved . "help"))
                        ))
    ("*Special*"   (and (saved . "space-star")
                        (not (saved . "stars"))))
    ("*Eww*"       (saved . "eww"))
    ("*Dired*"     (mode . dired-mode))
    ("*Configs*"   (or (projectile-root . "__config")
                       (saved . "addfiles")))
    ("*Help*"      (saved . "help"))
    ("Agendas"     (saved . "agendas"))
    ("Mail"        (saved . "mail"))
    ;; ("*Project: writing" (projectile-root . "jgrey4296.github.io"))
    ("Lisp"        (saved . "lisp"))
    ("Python"      (saved . "python"))
    ("Rust"        (saved . "rust"))
    ("Elixir"      (saved . "elixir"))
    ("Kotlin"      (saved . "kotlin"))
    ("Godot"       (saved . "godot"))
    ("DotNet"      (saved . "dotnet"))
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
  )

(speckler-add! ibuffer-groups () ;; starred, general
  :override t
  '(starred
    ("*Starred*"   (saved . "stars"))
    ("*Special*"   (saved . "special"))
    )
  '(general
    ("*Starred*"   (saved . "stars"))
    ("Org"         (saved . "org"))
    ("Programming" (saved . "programming"))
    ("Dired"       (saved . "dired"))
    )
  '(dired   ("dired"       (saved . "dired")))
  )

(speckler-add! ibuffer-groups () ;; text
  :override t
  '(text
    ("Org"         (saved . "org"))
    ("Markdown"    (mode  . markdown-mode))
    ("Rst"         (mode  . rst-mode))
    ("Tex"         (mode  . tex-mode))
    ("Bibtex"      (mode  . bibtex-mode))
    )
  )

(speckler-add! ibuffer-groups () ;; projects, programming, tests
  :override t
  '(projects
    ;; ("*Starred*" (starred-name))
    ("*Project: configs*" (projectile-root . "__config"))
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

(speckler-add! ibuffer-groups () ;; programing extension
  :extend t
  '(programming
    ("python" (or (derived-mode . python-mode) (derived-mode . python-base-mode)))
    ("lisp"   (derived-mode . emacs-lisp-mode))
    )
  )
