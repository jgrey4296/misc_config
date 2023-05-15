;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-
(doom-log "Setting up Misc Bindings")
(global-set-key (kbd "C-c u") #'universal-argument)

(define-key! read-expression-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element)

(map! :map global-map
      "<f1>" #'ignore
      "<f2>" #'ignore
      "<f3>" #'ignore
      "<f4>" #'ignore
      "<f5>" #'ignore
      "<f6>" #'ignore
      "<f10>" #'ignore
      )

;; For minibuffer use:
(map! :map ctl-x-map
      "[" "("
      "]" ")")

(map! :map universal-argument-map
      :prefix doom-leader-key     "u" #'universal-argument-more
      :prefix doom-leader-alt-key "u" #'universal-argument-more)

(map! :map special-mode-map
      :n "q" #'quit-window
      )


;;-- flycheck
(map! :map flycheck-error-list-mode-map
      :after flycheck
      :n "," nil
      :n "," #'tabulated-list-sort
      :n "{" #'tabulated-list-narrow-current-column
      :n "}" #'tabulated-list-widen-current-column
      )

(map! :leader
      :after flycheck
      :desc "Flycheck" "!" flycheck-command-map
      :prefix "c"
      :desc "Flycheck" "!" flycheck-command-map
      )
;;-- end flycheck

;;-- messages
(map! :after message
      :map messages-buffer-mode-map
      :g "0" #'evil-beginning-of-line
      )

;;-- end messages

;;-- evil overrides/intercept
(evil-make-overriding-map messages-buffer-mode-map)
(evil-make-intercept-map read-expression-map)

;;-- end evil overrides/intercept

;;-- Mouse Deactivation

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
;;-- end Mouse Deactivation

(map! :map calendar-mode-map
      :n "RET" #'+jg-bindings-calendar-insert-date
      )

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))
