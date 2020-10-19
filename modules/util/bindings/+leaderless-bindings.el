;;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-

(global-set-key (kbd "C-c [") '+jg-personal-insert-lparen)
(global-set-key (kbd "C-c ]") '+jg-personal-insert-rparen)

(map! :g "C-x h" help-map)

(map! :after (featurep! :editor evil)
 :m "TAB" nil
 :m "TAB" #'indent-for-tab-command
 )

(map! :after evil
      :map evil-motion-state-map
      "\\" nil)

(map! :after ibuffer
      :mode ibuffer-mode
      "\\" ibuffer--filter-map
      )

(map! :prefix "z"
      :nv "n" 'jg-toggle-narrow-buffer
      )

(map! :n "gb" #'avy-pop-mark)

(map! :after flycheck
      :map flycheck-error-list-mode-map
      "," nil
      :n "," #'tabulated-list-sort
      :n "{" #'tabulated-list-narrow-current-column
      :n "}" #'tabulated-list-widen-current-column
      )

(add-hook 'tabulated-list-mode-hook 'turn-off-evil-snipe-mode)
