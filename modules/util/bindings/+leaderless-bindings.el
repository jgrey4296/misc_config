;;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-

(global-set-key (kbd "C-c [") #'+jg-personal-insert-lparen)
(global-set-key (kbd "C-c ]") #'+jg-personal-insert-rparen)
(global-set-key (kbd "C-c u") #'universal-argument)
(map! :g "C-x h" help-map)


(map! :after (featurep! :editor evil)
 :m "TAB" nil
 :m "TAB" #'indent-for-tab-command
 )

(map! :after evil
      (:map evil-motion-state-map
      "\\" nil
      "] RET" #'jg-narrowing-move-focus-forward
      "[ RET" #'jg-narrowing-move-focus-backward)
      (:map evil-normal-state-map
      "z n" nil
      "z c" #'jg-toggle-narrow-buffer)
      )

(map! :after evil
      :map shell-mode-map
      :localleader
      "h" #'counsel-shell-history)

(map! :after ibuffer
      :mode ibuffer-mode
      "\\" ibuffer--filter-map
      )

(map! :after evil
      :map evil-normal-state-map
      :prefix "g"
      "b" #'avy-pop-mark)

(map! :after flycheck
      :map flycheck-error-list-mode-map
      :n "," nil
      :n "," #'tabulated-list-sort
      :n "{" #'tabulated-list-narrow-current-column
      :n "}" #'tabulated-list-widen-current-column
      )

(map! ;:after git-timemachine-mode
      :map git-timemachine-mode-map
      :n "[ g" #'git-timemachine-show-previous-revision
      :n "] g" #'git-timemachine-show-next-revision
      )

(add-hook 'tabulated-list-mode-hook 'turn-off-evil-snipe-mode)

(after! evil
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
)
