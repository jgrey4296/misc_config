;;; +vars.el -*- lexical-binding: t; -*-

;;-- xml commands
(setq jg-xml-xpath-command-string  "xmllint --pretty 2 --htmlout --xpath %s %s"
      jg-xml-format-command-string "xmllint --format %s"
      jg-xml-xidel-command-string  "xidel -s --output-format=xml --xpath \"%s\" %s"

      jg-xml-xpath-results-buffer-name "*xpath result*"
      jg-xml-xmllint-shell-buffer-name "*xmllint*"

      )

;;-- end xml commands

(setq mailcap-mime-extensions nil)
(pushnew! mailcap-mime-extensions '(".xhtml" . "text/html"))
(pushnew! mailcap-mime-extensions '(".xml" . "text/html"))

;;-- popup

(spec-handling-add! popup nil
                    ('nxml
                          ((,jg-xml-xpath-results-buffer-name :side bottom :ttl nil :height 0.3 :quit t :select nil :priority 100)
                           (,jg-xml-xmllint-shell-buffer-name :side right  :ttl nil :width  0.3 :quit t :select t   :priority 100)
                           )
                          )
                    )

;;-- end popup

;;-- fold spec
(spec-handling-add! fold nil
                      ('xml
                       :modes (web-mode)
                       :triggers (:open-all   nil
                                  :close-all  nil
                                  :toggle     web-mode-fold-or-unfold
                                  :open       nil
                                  :open-rec   nil
                                  :close      nil
                                  )
                       )
                      )
;;-- end fold spec

;;-- browse providers
(after! jg-ui-reapply-hook-ready
  (+jg-browse-add-lookup-spec 'xml
                              '(
                                ("MDN"                "https://developer.mozilla.org/en-US/search?q=%s")
                                )
                              )
  )

;;-- end browse providers
