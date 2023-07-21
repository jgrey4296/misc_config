;;; +bindings.el -*- lexical-binding: t; -*-

(evil-make-intercept-map shell-mode-map)
(evil-make-intercept-map comint-mode-map)

(map! :leader
      :desc "Pop Shell"             "'"   #'+jg-shell-new

      )


(map! :map shell-mode-map
      "C-d" #'comint-send-eof
      :localleader
      "h" #'counsel-shell-history
      )

;; overrides the default normal mode binding of evil-ret
(map! :map comint-mode-map
      :after comint
      "C-d" #'comint-send-eof
      :n "RET" #'comint-send-input
      )
