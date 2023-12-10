;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map pasp-mode-map
      :localleader
      :desc "Prettify" "p" #'prettify-symbols-mode
      :desc "Compile"  "c" #'pasp-run-buffer
      )

(map! :map jacamo-mode-map
      :localleader
      )

(map! :map prolog-mode-map
      :n "|" #'general-insert-call
      )
