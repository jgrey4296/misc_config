;;; editor/fold/+vars.el -*- lexical-binding: t; -*-

(setq code-shy-exclusions '(helm-major-mode
                            ivy-mode
                            minibuffer-mode
                            dired-mode
                            fundamental-mode
                            rst-mode
                            magit-status-mode
                            helpful-mode
                            shell-mode
                            ))

;;-- vimish-fold
(setq-default vimish-fold-dir (concat doom-cache-dir "vimish-fold/")
              vimish-fold-indication-mode 'right-fringe
              evil-vimish-fold-mode-map nil
              vimish-fold-header-width 50
              vimish-fold-persist-on-saving nil
              )

(spec-handling-add! fold
                    `(vimish
                     :modes (vimish-fold-mode)
                     :priority -50
                     :triggers (:delete     ,#'vimish-fold-delete
                                :open-all   ,#'vimish-fold-unfold-all
                                :close-all  ,#'vimish-fold-refold-all
                                :toggle     ,#'vimish-fold-toggle
                                :open       ,#'vimish-fold-unfold
                                :open-rec   nil
                                :close      ,#'vimish-fold-refold
                                )
                     )
                    )
;;-- end vimish-fold

;;-- hide show
(setq-default hs-hide-comments-when-hiding-all nil
              hs-set-up-overlay #'+fold-hideshow-set-up-overlay-fn
              )
(spec-handling-add! hideshow
                    '(default
                      ;;MODE START END COMMENT-START FORWARD-SEXP-FUNC ADJUST-BEG-FUNC
                      (vimrc-mode "{{{" "}}}" "\"")
                      (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>" "" "#" #'+fold-hideshow-forward-block-by-indent-fn nil)
                      (haml-mode "[#.%]" "\n" "/" #'+fold-hideshow-haml-forward-sexp-fn nil)
                      (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]" "end\\|[]}]" "#\\|=begin" #'ruby-forward-sexp)
                      (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch" "end" nil #'(lambda (_arg) (matlab-forward-sexp)))
                      (nxml-mode "<!--\\|<[^/>]*[^/]>"
                                 "-->\\|</[^/>]*[^/]>"
                                 "<!--" sgml-skip-tag-forward nil)
                      (latex-mode ;; LaTeX-find-matching-end needs to be inside the env
                       ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
                       "\\\\end{[a-zA-Z*]+}"
                       "%"
                       #'(lambda (_arg) ;; Don't fold whole document, that's useless
                         (unless (save-excursion
                                   (search-backward "\\begin{document}"
                                                    (line-beginning-position) t))
                           (LaTeX-find-matching-end)))
                       nil))
                      )

(spec-handling-add! fold
                    `(hide-show
                     :modes (hs-minor-mode)
                     :priority -25
                     :triggers (:open-all   ,#'hs-show-all
                                :close-all  ,#'hs-hide-all
                                :toggle     ,#'hs-toggle-hiding
                                :open       ,#'hs-show-block
                                :open-rec   nil
                                :close      ,#'hs-hide-block
                                )
                     )
                    )

;;-- end hide show

;;-- origami
(when (modulep! +origami)
  (spec-handling-add! fold
                      '(origami
                       :modes (origami-mode)
                       :priority -100
                       :triggers (:open-all   origami-open-all-nodes
                                  :close-all  origami-close-all-nodes
                                  :toggle     origami-toggle-node
                                  :open       origami-open-node
                                  :open-rec   origami-open-node-recursively
                                  :close      origami-close-node
                                  )
                       )
                     )
  )
;;-- end origami

;;-- outline
(setq outline-blank-line nil)

(spec-handling-add! fold
                    `(outline
                     :modes (outline-mode outline-minor-mode markdown-mode)
                     :priority -25
                     :triggers (:open-all   ,#'outline-show-all
                                :close-all  ,(cmd! (with-no-warnings (outline-hide-sublevels 1)))
                                :toggle     ,#'outline-toggle-children
                                :open       ,(cmd! (with-no-warnings (outline-show-entry) (outline-show-children)))
                                :open-rec   ,#'outline-show-subtree
                                :close      ,#'outline-hide-subtree
                                )
                     )
                    )
;;-- end outline

;;-- c like ifdef
(spec-handling-add! fold
                    '(ifdef
                     :modes (hide-ifdef-mode)
                     :priority -25
                     :triggers (:open-all   ,#'show-ifdefs
                                :close-all  ,#'hide-ifdefs
                                :toggle     nil
                                :open       ,#'show-ifdef-block
                                :open-rec   nil
                                :close      ,#'hide-ifdef-block
                                )
                     )
                    )
;;-- end c like ifdef

;;-- diff mode
(spec-handling-add! fold
                    '(vdiff
                     :modes (vdiff-mode vdiff-3way-mode)
                     :priority 200
                     :triggers (:open-all   ,#'vdiff-open-all-folds
                                :close-all  ,#'vdiff-close-all-folds
                                :toggle     ,#'vdiff-toggle-fold
                                :open       ,#'vdiff-open-fold
                                :open-rec   ,#'vdiff-open-fold
                                :close      ,#'vdiff-close-fold
                                )
                     )
                    )
;;-- end diff mode

;;-- reminder
;;   evil-fold-list = (list ((MODES) PROPERTIES))
;;   '((outline-minor-mode org-mode)
;;   - `:open-all' Open all folds.
;;   - `:close-all' Close all folds.
;;   - `:toggle' Toggle the display of the fold at point.
;;   - `:open' Open the fold at point.
;;   - `:open-rec' Open the fold at point recursively.
;;   - `:close' Close the fold at point.
;;-- end reminder
