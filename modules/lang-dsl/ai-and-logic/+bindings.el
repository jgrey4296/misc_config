;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map pasp-mode-map
      :after pasp-mode
      :localleader
      :desc "Prettify" "p" #'prettify-symbols-mode
      :desc "Compile"  "c" #'pasp-run-buffer
      )

(map! :map jacamo-mode-map
      :localleader
      :desc "Jacamo Github" :n "1" (cmd! (browse-url "https://github.com/jacamo-lang/jacamo"))
      :desc "Jacamo API" :n "3" (cmd! (browse-url "https://jacamo.sourceforge.net/doc/api/index.html"))
      :desc "Jason API"  :n "4" (cmd! (browse-url "https://jason.sourceforge.net/api/index.html"))

      )

(evil-make-intercept-map pasp-mode-map)
