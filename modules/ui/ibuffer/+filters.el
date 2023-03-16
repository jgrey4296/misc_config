;;; +filters.el -*- lexical-binding: t; -*-


;;-- ibuffer filters
(setq-default jg-ui-ibuffer-heuristics (rx (or "backtab"
                                               (regexp "\\.\\.")
                                               (regexp "^[[:alpha:]]\\{2,\\}")
                                               (regexp "which-key")
                                               (regexp "/ S")
                                               )
                                           )
              jg-ui-ibuffer-filters '()
              )

(add-to-list 'jg-ui-ibuffer-filters '("anti-[Helm|Magit|Help]" (not or
                                                                   (derived-mode . helm-major-mode)
                                                                   (derived-mode . helpful-mode)
                                                                   (derived-mode . magit-mode))))
(add-to-list 'jg-ui-ibuffer-filters '("Indirect-Window-Ring" (name . "->")))
(add-to-list 'jg-ui-ibuffer-filters '("default" (not or (used-mode . magit-diff-mode)
                                                    (used-mode . magit-process-mode)
                                                    (used-mode . magit-status-mode)
                                                    (used-mode . magit-diff-mode)
                                                    (used-mode . flycheck-error-list-mode)
                                                    (used-mode . helm-major-mode)
                                                    (used-mode . helpful-mode)
                                                    (used-mode . special-mode))))
(add-to-list 'jg-ui-ibuffer-filters '("Dired" (used-mode . dired-mode)))
(add-to-list 'jg-ui-ibuffer-filters '("star" (name . "^*")))
(add-to-list 'jg-ui-ibuffer-filters '("anti-helm-and-magit" (saved . "anti-magit") (saved . "anti-helm")))
(add-to-list 'jg-ui-ibuffer-filters '("anti-magit" (not derived-mode . magit-mode)))
(add-to-list 'jg-ui-ibuffer-filters '("git" (derived-mode . magit-mode) (saved . "anti-helm")))
(add-to-list 'jg-ui-ibuffer-filters '("bibtex" (used-mode . bibtex-mode)))
(add-to-list 'jg-ui-ibuffer-filters '("music" (or (name . "*\\(tidal\\|SCLang\\)")
                                                 (used-mode . sclang-mode)
                                                 (used-mode . tidal-mode)
                                                 (file-extension . "scd\\|hs\\|tidal"))))
(add-to-list 'jg-ui-ibuffer-filters '("org" (used-mode . org-mode)))
(add-to-list 'jg-ui-ibuffer-filters '("anti-helm" (not used-mode . helm-major-mode)))
(add-to-list 'jg-ui-ibuffer-filters '("python" (used-mode . python-mode)))
(add-to-list 'jg-ui-ibuffer-filters '("dired" (used-mode . dired-mode)))
(add-to-list 'jg-ui-ibuffer-filters '("programming" (or (derived-mode . prog-mode)
                                                       (mode . ess-mode)
                                                       (mode . compilation-mode))))
(add-to-list 'jg-ui-ibuffer-filters '("text document" (and (derived-mode . text-mode)
                                                          (not (starred-name)))))
(add-to-list 'jg-ui-ibuffer-filters '("TeX" (or (derived-mode . tex-mode)
                                               (mode . latex-mode)
                                               (mode . context-mode)
                                               (mode . ams-tex-mode)
                                               (mode . bibtex-mode))))
(add-to-list 'jg-ui-ibuffer-filters '("web" (or (derived-mode . sgml-mode)
                                               (derived-mode . css-mode)
                                               (mode . javascript-mode)
                                               (mode . js2-mode)
                                               (mode . scss-mode)
                                               (derived-mode . haml-mode)
                                               (mode . sass-mode))))
(add-to-list 'jg-ui-ibuffer-filters '("gnus" (or (mode . message-mode)
                                                (mode . mail-mode)
                                                (mode . gnus-group-mode)
                                                (mode . gnus-summary-mode)
                                                (mode . gnus-article-mode))))

;;-- end ibuffer filters

;;-- ibuffer filter groups
(setq jg-ui-ibuffer-filter-groups nil)

(add-to-list 'jg-ui-ibuffer-filter-groups '("default"
                                            ("Dired" (saved . "Dired"))
                                            ("Starred" (starred-name))
                                            ("Project: configs" (projectile-root "configs" . "/Volumes/documents/github/__configs/"))
                                            ("Project: writing" (projectile-root "writing" . "/Volumes/documents/github/jgrey4296.github.io/"))
                                            ("Project: Dropbox" (projectile-root "Dropbox" . "/Users/johngrey/Dropbox/"))))
(add-to-list 'jg-ui-ibuffer-filter-groups '("Dired"
                                            ("Dired" (used-mode . dired-mode))))
(add-to-list 'jg-ui-ibuffer-filter-groups '("my-default"
                                            ("star" (saved . "star"))
                                            ("org" (saved . "org"))
                                            ("programming" (saved . "programming"))
                                            ("dired" (saved . "dired"))))
(add-to-list 'jg-ui-ibuffer-filter-groups '("progorg"
                                            ("org" (saved . "org"))
                                            ("programming" (saved . "programming"))))
(add-to-list 'jg-ui-ibuffer-filter-groups '("programming"
                                            ("programming" (saved . "programming"))))
(add-to-list 'jg-ui-ibuffer-filter-groups '("dired"
                                            ("dired" (saved . "dired"))))
(add-to-list 'jg-ui-ibuffer-filter-groups '("org"
                                            ("org" (saved . "org"))))
(add-to-list 'jg-ui-ibuffer-filter-groups '("Home"
                                            ("helm-major-mode" (mode . helm-major-mode))
                                            ("log4e-mode" (mode . log4e-mode))
                                            ("prolog-mode" (mode . prolog-mode))
                                            ("emacs-lisp-mode" (mode . emacs-lisp-mode))
                                            ("org-mode" (mode . org-mode))
                                            ("text-mode" (mode . text-mode))
                                            ("dired-mode" (mode . dired-mode))
                                            ("debugger-mode" (mode . debugger-mode))))
;;-- end ibuffer filter groups
