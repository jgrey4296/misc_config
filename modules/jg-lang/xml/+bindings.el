;;; +bindings.el -*- lexical-binding: t; -*-

(evil-make-intercept-map nxml-mode-map)

(map! :map nxml-mode-map
      :desc "Run XPath Query" "?" #'+jg-xml-run-xpath
      )
