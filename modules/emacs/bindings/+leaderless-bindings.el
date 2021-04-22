;;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-

(global-set-key (kbd "C-c [") #'+jg-personal-insert-lparen)
(global-set-key (kbd "C-c ]") #'+jg-personal-insert-rparen)
(global-set-key (kbd "C-c u") #'universal-argument)

;; For minibuffer use:
(map! :map ctl-x-map
      "[" "("
      "]" ")")

(map! :g "C-x h" help-map)
(map! :map universal-argument-map
      :prefix doom-leader-key     "u"#'universal-argument-more
      :prefix doom-leader-alt-key "u"#'universal-argument-more)

;;
;; Shell
(map! :map shell-mode-map
      :localleader
      "h" #'counsel-shell-history)
;; Ibuffer
(map! :after ibuffer
      :mode ibuffer-mode
      "\\" ibuffer--filter-map
      )
;; Flycheck
(map! :after flycheck
      :map flycheck-error-list-mode-map
      :n "," nil
      :n "," #'tabulated-list-sort
      :n "{" #'tabulated-list-narrow-current-column
      :n "}" #'tabulated-list-widen-current-column
      )
;; Git Timemachine
(map! :map git-timemachine-mode-map
      :n "[ g" #'git-timemachine-show-previous-revision
      :n "] g" #'git-timemachine-show-next-revision
      )

;; Snipe
(map! :map evil-snipe-mode-map
      :nm "S" nil
      :nm "s" nil
      )

;; Mouse Deactivation
(define-key evil-motion-state-map [down-mouse-1] #'ignore)
(define-key evil-motion-state-map [mouse-1] #'ignore)
(define-key evil-motion-state-map [drag-mouse-1] #'ignore)

(define-key evil-motion-state-map [down-mouse-2] #'ignore)
(define-key evil-motion-state-map [mouse-2] #'ignore)
(define-key evil-motion-state-map [drag-mouse-2] #'ignore)

(define-key evil-motion-state-map [down-mouse-3] #'ignore)
(define-key evil-motion-state-map [mouse-3] #'ignore)
(define-key evil-motion-state-map [drag-mouse-3] #'ignore)

(define-key evil-motion-state-map [mouse-4] #'ignore)
(define-key evil-motion-state-map [mouse-5] #'ignore)
