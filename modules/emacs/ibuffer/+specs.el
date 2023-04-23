;;; +filters.el -*- lexical-binding: t; -*-

(require 'ibuf-ext)

(setq-default jg-ibuffer-heuristics (rx (or "backtab"
                                            (regexp "\\.\\.")
                                            (regexp "^[[:alpha:]]\\{2,\\}")
                                            (regexp "which-key")
                                            (regexp "/ S")
                                            )
                                        )
              )

;; Filters:

(spec-handling-add! ibuffer-filters nil
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
                                        ))
                               )
                    '(dired (used-mode . dired-mode))
                    )

(spec-handling-add! ibuffer-filters nil
                    '(window-ring (name . "->"))
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

(spec-handling-add! ibuffer-filters nil
                    '(mail (or (mode . message-mode)
                               (mode . mail-mode)
                               (mode . gnus-group-mode)
                               (mode . gnus-summary-mode)
                               (mode . gnus-article-mode))))

(spec-handling-add! ibuffer-filters nil
                   '(csv     ())
                   '(graphql ())
                   '(json    ())
                   '(logs    ())
                   '(toml    ())
                   '(xml     ())
                   '(yaml    ())
                    )

(spec-handling-add! ibuffer-filters nil
                    '(doot ())
                    )

;; Groups:

(spec-handling-add! ibuffer-groups nil
                    '(default  (("*Starred*"   (saved . "stars"))
                                ("*Project: configs" (projectile-root . "__configs"))
                                ("*Project: writing" (projectile-root . "jgrey4296.github.io"))
                                ))
                    '(starred (("*Starred*"   (saved . "stars"))))
                    '(General (("Starred"     (saved . "stars"))
                               ("org"         (saved . "org"))
                               ("programming" (saved . "programming"))
                               ("dired"       (saved . "dired"))))
                    '(org     (("org"         (saved . "org"))))
                    '(dired   (("dired"       (saved . "dired"))))
                    )

(spec-handling-add! ibuffer-groups nil
                    '(projects (("*Starred*" (starred-name))
                                ("*Project: configs*" (projectile-root . "__configs"))
                                ("*Project: modules*" (projectile-root . "modules"))
                                ("*Project: writing*" (projectile-root . "jgrey4296.github.io"))
                                ("*Project: Dropbox*" (projectile-root . "Dropbox"))
                                ))
                    '(programming (("programming" (saved . "programming"))))
                    )

(spec-handling-extend! ibuffer-groups nil
                       '(programming
                        ("python" (derived-mode . python-mode))
                        ("lisp"   (derived-mode . emacs-lisp-mode))
                        )
                       )
