;;; +filters.el -*- lexical-binding: t; -*-

(require 'ibuf-ext)

(define-ibuffer-filter workspace-buffers
    "Filter for workspace buffers"
  (:reader (+workspace-get (read-string "workspace name: "))
   :description "workspace")
  (memq buf (+workspace-buffer-list qualifier)))

(setq-default jg-ibuffer-heuristics (rx (or "backtab"
                                            (regexp "\\.\\.")
                                            (regexp "^[[:alpha:]]\\{2,\\}")
                                            (regexp "which-key")
                                            (regexp "/ S")
                                            )
                                        )
              )

;; Filters:

(+jg-ibuffer-add-filter-spec 'default
                             "-clutter" '(not (or (name . "*http ")
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
                                                  ))
                             )

(+jg-ibuffer-add-filter-spec 'dired
                            "dired" '(used-mode . dired-mode)
                            )

(+jg-ibuffer-add-filter-spec 'window-ring "Window-Ring" '(name . "->"))
(+jg-ibuffer-add-filter-spec 'stars       "star"        '(name . "^*"))
(+jg-ibuffer-add-filter-spec 'music       "music"       '(or (name . "*\\(tidal\\|SCLang\\)")
                                                            (used-mode . sclang-mode)
                                                            (used-mode . tidal-mode)
                                                            (file-extension . "scd\\|hs\\|tidal"))
                            )

(+jg-ibuffer-add-filter-spec 'org         "org"         '(derived-mode . org-mode))
(+jg-ibuffer-add-filter-spec 'programming
                            "python"      '(used-mode . python-mode)
                            "programming" '(or (derived-mode . prog-mode)
                                               (mode . ess-mode)
                                               (mode . compilation-mode))
                            )
(+jg-ibuffer-add-filter-spec 'text
                            "text"        '(and (derived-mode . text-mode) (not (starred-name)))
                            "TeX"         '(or (derived-mode . tex-mode)
                                               (mode . latex-mode)
                                               (mode . context-mode)
                                               (mode . ams-tex-mode))
                            "bibtex"      '(used-mode . bibtex-mode)
                            "web"         '(or (derived-mode . sgml-mode)
                                               (derived-mode . css-mode)
                                               (mode . javascript-mode)
                                               (mode . js2-mode)
                                               (mode . scss-mode)
                                               (derived-mode . haml-mode)
                                               (mode . sass-mode))
                            )
(+jg-ibuffer-add-filter-spec 'mail "mail" '(or (mode . message-mode)
                                              (mode . mail-mode)
                                              (mode . gnus-group-mode)
                                              (mode . gnus-summary-mode)
                                              (mode . gnus-article-mode))
                            )

(+jg-ibuffer-add-filter-spec 'data
                            "csv"     '()
                            "graphql" '()
                            "json"    '()
                            "logs"    '()
                            "toml"    '()
                            "xml"     '()
                            "yaml"    '()
                            )

(+jg-ibuffer-add-filter-spec 'doot
                            "dooter" '()
                            "doot"   '()
                            )

;; Groups:

(+jg-ibuffer-add-group-spec 'default
                           "default"  '(("*Starred*"   (saved . "star"))
                                        ("*Project: configs" (projectile-root . "__configs"))
                                        ("*Project: writing" (projectile-root . "jgrey4296.github.io"))
                                        )
                           "Starred"  '(("*Starred*"   (saved . "star")))
                           "General"  '(("Starred"     (saved . "star"))
                                        ("org"         (saved . "org"))
                                        ("programming" (saved . "programming"))
                                        ("dired"       (saved . "dired")))
                           "org"      '(("org"         (saved . "org")))
                           "dired"    '(("dired"       (saved . "dired")))

                           )
(+jg-ibuffer-add-group-spec 'projects
                           "projects" '(("*Starred*" (starred-name))
                                        ("*Project: configs*" (projectile-root . "__configs"))
                                        ("*Project: modules*" (projectile-root . "modules"))
                                        ("*Project: writing*" (projectile-root . "jgrey4296.github.io"))
                                        ("*Project: Dropbox*" (projectile-root . "Dropbox"))
                                        )
                           )
(+jg-ibuffer-add-group-spec 'programming
                           "programming" '(
                                           ("programming" (saved . "programming"))
                                           )
                           )

(+jg-ibuffer-extend-group 'programming "programming"
                          '("python" (derived-mode . python-mode))
                          '("lisp"   (derived-mode . emacs-lisp-mode))
                          )
