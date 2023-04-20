;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map pasp-mode-map
      :after pasp-mode
      :localleader
      :desc "Prettify" "p" #'prettify-symbols-mode
      :desc "Compile"  "c" #'pasp-run-buffer
      )

(map! :map jacamo-mode-map
      :localleader
      )

(evil-make-intercept-map pasp-mode-map)
