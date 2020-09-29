;;; editor/fold/+bindings.el -*- lexical-binding: t; -*-

(map! :map evil-normal-state-map
      :prefix "z"
      (:prefix ("v" . "Vimish-fold")
      "a" 'vimish-fold-toggle
      "A" 'vimish-fold-toggle-all
      "f" 'vimish-fold
      "d" 'vimish-fold-delete
      "D" 'vimish-fold-delete-all
      "j" 'vimish-fold-next-fold
      "k" 'vimish-fold-previous-fold
      "m" 'vimish-fold-refold-all
      "r" 'vimish-fold-unfold-all
      "o" 'outline-hide-other)

      "o" nil
      (:prefix ("o" . "Outline")
       "a" 'outline-toggle-children
       "A" 'outline-show-all
       "f" 'outline-hide-subtree
       "m" #'(lambda () (interactive) (outline-hide-region-body (point-min) (point-max)))
       "o" 'outline-hide-other
       )

      (:prefix ("s" . "Hide-show")
       "a" 'hs-toggle-hiding
       "A" 'hs-show-all
       "m" 'hs-hide-all
       "f" 'hs-hide-block-at-point
       )

)

(evil-define-key* 'motion 'global
  "zj" #'+fold/next
  "zk" #'+fold/previous
  "zA" #'outline-show-subtree)

;; (when (featurep! :editor evil)
;;   ;; Add vimish-fold, outline-mode & hideshow support to folding commands
;;   (define-key! 'global
;;     [remap evil-toggle-fold]   #'+fold/toggle
;;     [remap evil-close-fold]    #'+fold/close
;;     [remap evil-open-fold]     #'+fold/open
;;     [remap evil-open-fold-rec] #'+fold/open
;;     [remap evil-close-folds]   #'+fold/close-all
;;     [remap evil-open-folds]    #'+fold/open-all)
;;   )
