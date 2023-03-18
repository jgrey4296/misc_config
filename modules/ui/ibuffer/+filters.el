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

(+jg-ibuffer-define-filters 'default
                            "clutter" '(not (or (used-mode . magit-diff-mode)
                                                (used-mode . magit-process-mode)
                                                (used-mode . magit-status-mode)
                                                (used-mode . magit-diff-mode)
                                                (used-mode . flycheck-error-list-mode)
                                                (used-mode . helm-major-mode)
                                                (used-mode . helpful-mode)
                                                (used-mode . special-mode)
                                                (derived-mode . helm-major-mode)
                                                (derived-mode . helpful-mode)
                                                (derived-mode . magit-mode)))
                            )

(+jg-ibuffer-define-filters 'dired :override
                            "dired" '(used-mode . dired-mode)
                            )

(+jg-ibuffer-define-filters 'window-ring "Window-Ring" '(name . "->"))
(+jg-ibuffer-define-filters 'stars       "star"        '(name . "^*"))
(+jg-ibuffer-define-filters 'music       "music"       '(or (name . "*\\(tidal\\|SCLang\\)")
                                                            (used-mode . sclang-mode)
                                                            (used-mode . tidal-mode)
                                                            (file-extension . "scd\\|hs\\|tidal"))
                            )

(+jg-ibuffer-define-filters 'org         "org"         '(used-mode . org-mode))
(+jg-ibuffer-define-filters 'programming
                            "python"      '(used-mode . python-mode)
                            "programming" '(or (derived-mode . prog-mode)
                                               (mode . ess-mode)
                                               (mode . compilation-mode))
                            )
(+jg-ibuffer-define-filters 'text
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
(+jg-ibuffer-define-filters 'mail "mail" '(or (mode . message-mode)
                                              (mode . mail-mode)
                                              (mode . gnus-group-mode)
                                              (mode . gnus-summary-mode)
                                              (mode . gnus-article-mode))
                            )

(+jg-ibuffer-define-filters 'data
                            "csv"     '()
                            "graphql" '()
                            "json"    '()
                            "logs"    '()
                            "toml"    '()
                            "xml"     '()
                            "yaml"    '()
                            )

(+jg-ibuffer-define-filters 'doot
                            "dooter" '()
                            "doot"   '()
                            )

;; Groups:

(+jg-ibuffer-define-groups 'dired "dired" '("dired" (saved . "dired")))
(+jg-ibuffer-define-groups 'default :override
                           "projects" '(and ("Starred" (starred-name))
                                            ("Project: configs" (projectile-root "configs" . "/Volumes/documents/github/__configs/"))
                                            ("Project: writing" (projectile-root "writing" . "/Volumes/documents/github/jgrey4296.github.io/"))
                                            ("Project: Dropbox" (projectile-root "Dropbox" . "/Users/johngrey/Dropbox/")))
                           "other"    '(and ("star" (saved . "star"))
                                            ("org" (saved . "org"))
                                            ("programming" (saved . "programming"))
                                            ("dired" (saved . "dired")))
                           "programming" '("programming" (saved . "programming"))
                           "org"         '("org" (saved . "org"))
                           )
(provide 'jg-ibuffer-filters-init)
