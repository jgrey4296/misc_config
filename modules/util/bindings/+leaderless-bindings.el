;;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-

(global-set-key (kbd "C-c [") #'+jg-personal-insert-lparen)
(global-set-key (kbd "C-c ]") #'+jg-personal-insert-rparen)
(global-set-key (kbd "C-c u") #'universal-argument)
(map! :g "C-x h" help-map)
(map! :map universal-argument-map
      :prefix doom-leader-key     "u"#'universal-argument-more
      :prefix doom-leader-alt-key "u"#'universal-argument-more)

;; Evil States
(map! :after evil
      :m "TAB" nil
      :m "TAB" #'indent-for-tab-command
      (:map evil-motion-state-map
      "\\" nil
      "] RET" #'jg-narrowing-move-focus-forward
      "[ RET" #'jg-narrowing-move-focus-backward)
      (:map evil-normal-state-map
      "z n" nil
      "z c" #'jg-toggle-narrow-buffer)
      (:map evil-normal-state-map
       :prefix "g"
       "b" #'avy-pop-mark
       ">" #'evil-goto-column)
)
;; Shell
(map! :after evil
      :map shell-mode-map
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
