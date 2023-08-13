;;; +bindings.el -*- lexical-binding: t; -*-

(after! nxml-mode
  (evil-make-intercept-map nxml-mode-map)
  )
(after! mhtml-mode
  (evil-make-intercept-map mhtml-mode-map)
)

(map! :map jg-dired-mode-map
      :localleader
      :desc "Query Xml" :n "1"   #'+jg-xml-dired-run-xidel
      :desc "Manifest"  :n "i m" #'+jg-xml-epub-manifest-generate
      )
(map! :map jg-dired-mode-map
      :prefix ("> x" . "Xml/Json Utils")
      :desc "Query"    "?" #'+jg-xml-dired-run-xidel

      :desc "Elements"        "e" #'+jg-xml-dired-elements
      :desc "Select"          "q" #'+jg-xml-dired-select
      :desc "Validate"        "v" #'+jg-xml-dired-validate
      :desc "Format"          "f" #'+jg-xml-dired-format
      :desc "Schema"          "s" #'+jg-xml-dired-generate-schema
      :desc "Schema Uml"      "u" #'+jg-xml-dired-schema-uml
      :desc "Generate Python" "p" #'+jg-xml-dired-gen-python
      :desc "Visualise Json"  "j" '+jg-xml-dired-visualise-json
      )

(map! :map nxml-mode-map
      :after nxml-mode
      "/" nil
      :i "/" #'nxml-electric-slash
      :n "=" #'+jg-xml-format-buffer
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :localleader
      :desc "XSD Reference"   :n "1" (cmd! (browse-url "https://www.w3schools.com/xml/schema_intro.asp"))
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :desc "Load into shell" :n "l" #'+jg-xml-load-into-repl
      :desc "Validate"        :n "v" #'+jg-xml-validate
      )

(map! :map (nxml-mode-map mhtml-mode-map web-mode-map)
      :n "!" (cmd! (eww-open-file (buffer-file-name)))
      )

(map! :map mhtml-mode-map
      :after mhtml-mode
      "/" nil
      :i "/" #'sgml-slash
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :localleader
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :desc "Load into shell" :n "l" #'+jg-xml-load-into-repl
      )

(map! :map (web-mode-map so-long-mode-map)
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :localleader
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :desc "Load into shell" :n "l" #'+jg-xml-load-into-repl
      :desc "Html Reference"  :n "1" (cmd! (browse-url "https://developer.mozilla.org/en-US/docs/Web/HTML"))
      )
