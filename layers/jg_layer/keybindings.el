;; jg_layer keybindings.el
;; loaded fifth

;;(configuration-layer/declare-layer)
;;(configuration-layer/declare-layers)
;;(configuration-layer/layer-usedp)
;;(configuration-layer/package-usedp)

(global-set-key (kbd "C-c [") 'jg_layer/insert-lparen)
(global-set-key (kbd "C-c ]") 'jg_layer/insert-rparen)
(spacemacs/set-leader-keys
  ;; Registers:
  "r v" 'view-register
  "r l" 'list-registers
  "r w" 'window-configuration-to-register
  "r x" 'copy-to-register
  "r j" 'jump-to-register
  "r f" 'frameset-to-register
  "r i" 'insert-register
  ;; Expression:
  "x e" 'eval-expression
  ;; Clearing
  "b c" 'jg_layer/clear-buffer
  )
