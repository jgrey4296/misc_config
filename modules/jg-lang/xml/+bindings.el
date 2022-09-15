;;; +bindings.el -*- lexical-binding: t; -*-

(evil-make-intercept-map nxml-mode-map)
(evil-make-intercept-map mhtml-mode-map)

(map! :map nxml-mode-map
      :after 'nxml-mode
      :desc "Run XPath Query" "?" #'+jg-xml-run-xpath
      :localleader
      :desc "Run XPath Query" "?" #'+jg-xml-run-xpath
      )

(map! :map mhtml-mode-map
      ;; :after 'mhtml-mode
      :desc "Run XPath Query" "?" #'+jg-xml-run-xpath
      :localleader
      :desc "Run XPath Query" "?" #'+jg-xml-run-xpath
      )
