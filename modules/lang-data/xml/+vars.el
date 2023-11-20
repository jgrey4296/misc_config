;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-xml-elements-command-string        "xmlstarlet el -u *")
(defvar jg-xml-format-command-basic           "xmlstarlet fo ")
(defvar jg-xml-format-command-string          "xmllint --format %s")
(defvar jg-xml-python-generate-command-string "xsdata generate -r -p %s --relative-imports --postponed-annotations ?")
(defvar jg-xml-repl-command                   "xmllint")
(defvar jg-xml-schema-command-string          "trang * %s.xsd")
(defvar jg-xml-schema-plantuml-command-string "xsdata generate -o plantuml -pp * > schema.pu")
(defvar jg-xml-select-command-string          "xmlstarlet sel -t %s -n *")
(defvar jg-xml-validate-command-basic         "xmlstarlet val ")
(defvar jg-xml-validate-command-string        "xmlstarlet val -e -s %s %s")
(defvar jg-xml-wrap-json-command-string       "cat ? | awk 'BEGIN {print \"@startjson\"} END {print \"@endjson\"} {print $0}' | plantuml -p > `?`.png ")
(defvar jg-xml-xidel-command-string           "xidel -s --output-format=xml --xpath \"%s\" %s")
(defvar jg-xml-xpath-command-string           "xmlstarlet lint --pretty 2 --htmlout --xpath %s %s")

(defvar jg-xml-xpath-results-buffer-name      "*xpath result*")
(defvar jg-xml-xmllint-shell-buffer-name      "*xmllint*")

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
                    '(nxml-mode (:mode company-nxml))
                    )

(spec-handling-add! auto-modes
                    '(xml
                      ("\\.p\\(?:list\\|om\\)\\'" . nxml-mode)
                      ("\\.xs\\(?:d\\|lt\\)\\'"   . nxml-mode)
                      ( "\\.rss\\'" . nxml-mode)
                      )
                    )
(spec-handling-add! eval
                    `(nxml-mode
                      :start ,#'+xml/open-repl
                      )
                    `(mhtml-mode
                      :start ,#'+xml/open-repl)
                    )
;;-- end specs
