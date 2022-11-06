;;; +bindings.el -*- lexical-binding: t; -*-

(after! nxml-mode
  (evil-make-intercept-map nxml-mode-map)
  )
(after! mhtml-mode
  (evil-make-intercept-map mhtml-mode-map)
)

(map! :map dired-mode-map
      :after dired
      :localleader
      :desc "Query Xml" :n "1"   #'+jg-xml-dired-run-xidel
      :desc "Manifest"  :n "d m" #'+jg-xml-epub-manifest-generate
      )

(map! :map nxml-mode-map
      :after nxml-mode
      "/" nil
      :i "/" #'nxml-electric-slash
      :n "=" #'+jg-xml-format-buffer
      :desc "Run XPath Query" :n "?" #'+jg-xml-run-xidel
      :localleader
      :desc "XSD Reference"   :n "1" (cmd! (+jg-misc-browse-url "https://www.w3schools.com/xml/schema_intro.asp"))
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
      :desc "Html Reference"  :n "1" (cmd! (+jg-misc-browse-url "https://developer.mozilla.org/en-US/docs/Web/HTML"))
      )

(map! :map css-mode-map
      :localleader
      :desc "CSS Reference"   :n "1" (cmd! (+jg-misc-browse-url "https://developer.mozilla.org/en-US/docs/Web/CSS"))
      )

(map! :map less-css-mode-map
      :localleader
       :desc "LESS Reference"  :n "1" (cmd! (+jg-misc-browse-url "https://lesscss.org/features/"))
       :desc "Compile Less"    :n "c" #'less-css-compile
       )
