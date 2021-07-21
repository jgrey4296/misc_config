;;; editor/fold/+vars.el -*- lexical-binding: t; -*-

(after! evil-vimish-fold
  (setq evil-fold-list
        `(
          ((vdiff-mode)
           :open-all   vdiff-open-all-folds
           :close-all  vdiff-close-all-folds
           :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
           :open       ,(lambda () (call-interactively 'vdiff-open-fold))
           :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
           :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
          ((vdiff-3way-mode)
           :open-all   vdiff-open-all-folds
           :close-all  vdiff-close-all-folds
           :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
           :open       ,(lambda () (call-interactively 'vdiff-open-fold))
           :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
           :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
          ((hide-ifdef-mode)
           :open-all   show-ifdefs
           :close-all  hide-ifdefs
           :toggle     nil
           :open       show-ifdef-block
           :open-rec   nil
           :close      hide-ifdef-block)
          ((emacs-lisp-mode lisp-mode)
           :open-all   hs-show-all
           :close-all  hs-hide-all
           :toggle     hs-toggle-hiding
           :open       hs-show-block
           :open-rec   nil
           :close      hs-hide-block)
          ((outline-mode outline-minor-mode org-mode markdown-mode)
           :open-all   outline-show-all
           :close-all  ,(lambda ()
                          (with-no-warnings (outline-hide-sublevels 1)))
           :toggle     outline-toggle-children
           :open       ,(lambda ()
                          (with-no-warnings
                            (outline-show-entry)
                            (outline-show-children)))
           :open-rec   outline-show-subtree
           :close      outline-hide-subtree)
          ((origami-mode)
           :open-all   ,(lambda () (origami-open-all-nodes (current-buffer)))
           :close-all  ,(lambda () (origami-close-all-nodes (current-buffer)))
           :toggle     ,(lambda () (origami-toggle-node (current-buffer) (point)))
           :open       ,(lambda () (origami-open-node (current-buffer) (point)))
           :open-rec   ,(lambda () (origami-open-node-recursively (current-buffer) (point)))
           :close      ,(lambda () (origami-close-node (current-buffer) (point))))
          ((hs-minor-mode)
           :open-all   hs-show-all
           :close-all  hs-hide-all
           :toggle     hs-toggle-hiding
           :open       hs-show-block
           :open-rec   nil
           :close      hs-hide-block)
          ((vimish-fold-mode)
           :delete     vimish-fold-delete
           :open-all   vimish-fold-unfold-all
           :close-all  vimish-fold-refold-all
           :toggle     vimish-fold-toggle
           :open       vimish-fold-unfold
           :open-rec   nil
           :close      vimish-fold-refold)
          )
        )
)


;;   ((MODES) PROPERTIES)
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
