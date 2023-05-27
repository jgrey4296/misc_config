;;; +vars.el -*- lexical-binding: t; -*-

(setq jg-xml-xpath-command-string  "xmllint --pretty 2 --htmlout --xpath %s %s"
      jg-xml-format-command-string "xmllint --format %s"
      jg-xml-xidel-command-string  "xidel -s --output-format=xml --xpath \"%s\" %s"

      jg-xml-xpath-results-buffer-name "*xpath result*"
      jg-xml-xmllint-shell-buffer-name "*xmllint*"

      )

(setq mailcap-mime-extensions nil)
(pushnew! mailcap-mime-extensions '(".xhtml" . "text/html"))
(pushnew! mailcap-mime-extensions '(".xml" . "text/html"))

;;-- specs

(spec-handling-add! popup
                    `(nxml
                      (,jg-xml-xpath-results-buffer-name :side bottom :ttl nil :height 0.3 :quit t :select nil :priority 100)
                      (,jg-xml-xmllint-shell-buffer-name :side right  :ttl nil :width  0.3 :quit t :select t   :priority 100)
                      )
                    )

(spec-handling-add! lookup-url
                    '(xml
                     ("Firefox Dev" "https://developer.mozilla.org/en-US/search?q=%s")
                     )
                    )
(spec-handling-add! company
                    '(nxml-mode (:mode . #'company-nxml))
                    )

(set-repl-handler! 'nxml-mode #'+xml/open-repl)
(set-repl-handler! 'mhtml-mode #'+xml/open-repl)
;;-- end specs
