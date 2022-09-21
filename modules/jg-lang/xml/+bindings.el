;;; +bindings.el -*- lexical-binding: t; -*-

(evil-make-intercept-map nxml-mode-map)
(evil-make-intercept-map mhtml-mode-map)

(map! :map dired-mode-map
      :after dired
      :localleader
      :desc "Query Xml" :n "1"   #'+jg-xml-dired-run-xidel
      :desc "Manifest"  :n "d m" #'+jg-xml-epub-manifest-generate
      )

(map! :map nxml-mode-map
      ;; :after 'nxml-mode
      "/" nil
      :i "/" #'nxml-electric-slash
      :n "=" #'+jg-xml-format-buffer
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :localleader
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :desc "Load into shell" :n "l" #'+jg-xml-load-into-repl
      )

(map! :map mhtml-mode-map
      ;; :after 'mhtml-mode
      "/" nil
      :i "/" #'sgml-slash
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :localleader
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :desc "Load into shell" :n "l" #'+jg-xml-load-into-repl
      )

(map! :map web-mode-map
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :localleader
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :desc "Load into shell" :n "l" #'+jg-xml-load-into-repl
      )
