;;; lang/python/+bindings.el -*- lexical-binding: t; -*-

;; (map! :map python-mode-map
;;       :n "z d" '+jg-personal-toggle-all-defs
;;       :n "z D" '+jg-personal-close-class-defs
;;       :leader
;;       (:prefix "i"
;;        :n "d" #'+jg-personal-python-toggle-breakpoint
;;        )
;;       (:prefix "j"
;;        :n "C" 'helm-gtags-create-tags
;;        :n "d" 'helm-gtags-find-tag
;;        :n "D" 'helm-gtags-find-tag-other-window
;;        :n "G" 'helm-gtags-dwim-other-window
;;        :n "i" 'helm-gtags-tags-in-this-function
;;        :n "l" 'helm-gtags-parse-file
;;        :n "n" 'helm-gtags-next-history
;;        :n "p" 'helm-gtags-previous-history
;;        :n "r" 'helm-gtags-find-rtag
;;        :n "R" 'helm-gtags-resume
;;        :n "s" 'helm-gtags-select
;;        :n "S" 'helm-gtags-show-stack
;;        :n "y" 'helm-gtags-find-symbol
;;        :n "U" 'helm-gtags-update-tags
;;        )
;;         )

(progn
  (general-define-key :states 'normal :keymaps
                      '(python-mode-map)
                      "z d" '+jg-personal-toggle-all-defs "z D" '+jg-personal-close-class-defs)
  (doom--define-leader-key :states 'normal :keymaps
                           '(python-mode-map)
                           :infix "i" "d" #'+jg-personal-python-toggle-breakpoint)
  (doom--define-leader-key :states 'normal :keymaps
                           '(python-mode-map)
                           :infix "j" "C" 'helm-gtags-create-tags "d" 'helm-gtags-find-tag "D" 'helm-gtags-find-tag-other-window "G" 'helm-gtags-dwim-other-window "i" 'helm-gtags-tags-in-this-function "l" 'helm-gtags-parse-file "n" 'helm-gtags-next-history "p" 'helm-gtags-previous-history "r" 'helm-gtags-find-rtag "R" 'helm-gtags-resume "s" 'helm-gtags-select "S" 'helm-gtags-show-stack "y" 'helm-gtags-find-symbol "U" 'helm-gtags-update-tags))
