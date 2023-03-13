;;; editor/fold/+vars.el -*- lexical-binding: t; -*-

(setq autohide-minor-mode-exclusions '(
                                       helm-major-mode
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
              jg-fold-vimish-spec '((vimish-fold-mode)
                                    :delete     vimish-fold-delete
                                    :open-all   vimish-fold-unfold-all
                                    :close-all  vimish-fold-refold-all
                                    :toggle     vimish-fold-toggle
                                    :open       vimish-fold-unfold
                                    :open-rec   nil
                                    :close      vimish-fold-refold)
              vimish-fold-header-width 50
              vimish-fold-persist-on-saving nil
              )
;;-- end vimish-fold

;;-- hide show
(setq-default hs-hide-comments-when-hiding-all nil
              hs-set-up-overlay #'+fold-hideshow-set-up-overlay-fn
              jg-fold-hs-spec `((hs-minor-mode emacs-lisp-mode lisp-mode)
                                :open-all   hs-show-all
                                :close-all  hs-hide-all
                                :toggle     hs-toggle-hiding
                                :open       hs-show-block
                                :open-rec   nil
                                :close      hs-hide-block)

              ;; extra folding support for more languages
              hs-special-modes-alist
              (append
               '((vimrc-mode "{{{" "}}}" "\"")
                 (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                            ""
                            "#"
                            +fold-hideshow-forward-block-by-indent-fn nil)
                 (haml-mode "[#.%]" "\n" "/" +fold-hideshow-haml-forward-sexp-fn nil)
                 (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
                            "end\\|[]}]"
                            "#\\|=begin"
                            ruby-forward-sexp)
                 (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
                              "end"
                              nil (lambda (_arg) (matlab-forward-sexp)))
                 (nxml-mode "<!--\\|<[^/>]*[^/]>"
                            "-->\\|</[^/>]*[^/]>"
                            "<!--" sgml-skip-tag-forward nil)
                 (latex-mode
                  ;; LaTeX-find-matching-end needs to be inside the env
                  ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
                  "\\\\end{[a-zA-Z*]+}"
                  "%"
                  (lambda (_arg)
                    ;; Don't fold whole document, that's useless
                    (unless (save-excursion
                              (search-backward "\\begin{document}"
                                               (line-beginning-position) t))
                      (LaTeX-find-matching-end)))
                  nil))
               )
  )
;;-- end hide show

;;-- origami
(setq jg-fold-origami-spec `((origami-mode)
                             :open-all   ,(lambda () (origami-open-all-nodes (current-buffer)))
                             :close-all  ,(lambda () (origami-close-all-nodes (current-buffer)))
                             :toggle     ,(lambda () (origami-toggle-node (current-buffer) (point)))
                             :open       ,(lambda () (origami-open-node (current-buffer) (point)))
                             :open-rec   ,(lambda () (origami-open-node-recursively (current-buffer) (point)))
                             :close      ,(lambda () (origami-close-node (current-buffer) (point)))))
;;-- end origami

;;-- outline
(setq jg-fold-outline-spec `((outline-mode outline-minor-mode org-mode markdown-mode)
                             :open-all   outline-show-all
                             :close-all  ,(lambda ()
                                            (with-no-warnings (outline-hide-sublevels 1)))
                             :toggle     outline-toggle-children
                             :open       ,(lambda ()
                                            (with-no-warnings
                                              (outline-show-entry)
                                              (outline-show-children)))
                             :open-rec   outline-show-subtree
                             :close      outline-hide-subtree))
;;-- end outline

;;-- c like ifdef
(setq jg-fold-ifdef-spec `((hide-ifdef-mode)
                           :open-all   show-ifdefs
                           :close-all  hide-ifdefs
                           :toggle     nil
                           :open       show-ifdef-block
                           :open-rec   nil
                           :close      hide-ifdef-block))
;;-- end c like ifdef

;;-- diff mode
(setq jg-fold-diff-spec `((vdiff-mode vdiff-3way-mode)
                          :open-all   vdiff-open-all-folds
                          :close-all  vdiff-close-all-folds
                          :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
                          :open       ,(lambda () (call-interactively 'vdiff-open-fold))
                          :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
                          :close      ,(lambda () (call-interactively 'vdiff-close-fold))))
;;-- end diff mode

;;-- all together
(setq jg-fold-vars-fold-list (list
                              jg-fold-diff-spec
                              jg-fold-ifdef-spec
                              jg-fold-outline-spec
                              jg-fold-origami-spec
                              jg-fold-hs-spec
                              jg-fold-vimish-spec
                              ))
(setq evil-fold-list jg-fold-vars-fold-list)
;;-- end all together

(provide 'jg-fold-specs)

;;-- reminder
;;   evil-fold-list = (list ((MODES) PROPERTIES))
;;
;; MODES acts as a predicate, containing the symbols of all major or
;; minor modes for which the handler should match.  For example:
;;
;;   '((outline-minor-mode org-mode) ...)
;;
;; PROPERTIES specifies possible folding actions and the functions to be
;; applied in the event of a match on one (or more) of the MODES; the
;; supported properties are:
;;
;;   - `:open-all'
;;     Open all folds.
;;   - `:close-all'
;;     Close all folds.
;;   - `:toggle'
;;     Toggle the display of the fold at point.
;;   - `:open'
;;     Open the fold at point.
;;   - `:open-rec'
;;     Open the fold at point recursively.
;;   - `:close'
;;     Close the fold at point.
;;
;; Each value must be a function.  A value of `nil' will cause the action
;; to be ignored for that respective handler.  For example:
;;
;;   `((org-mode)
;;      :close-all  nil
;;      :open       ,(lambda ()
;;                     (show-entry)
;;                     (show-children))
;;      :close      hide-subtree)
;;
;;-- end reminder
