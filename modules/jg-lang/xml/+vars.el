;;; +vars.el -*- lexical-binding: t; -*-

(setq jg-xml-xpath-command-string  "xmllint --pretty 2 --htmlout --xpath %s %s"
      jg-xml-format-command-string "xmllint --format %s"
      jg-xml-xidel-command-string  "xidel -s --output-format=xml --xpath \"%s\" %s"

      jg-xml-xpath-results-buffer-name "*xpath result*"
      jg-xml-xmllint-shell-buffer-name "*xmllint*"

      )

(pushnew! mailcap-mime-extensions '(".xhtml" . "text/html"))
(pushnew! mailcap-mime-extensions '(".xml" . "text/html"))
;;-- popup
(after! 'jg-popup-init
  (+jg-ui-popup-add-rules 'nxml
                          `((,jg-xml-xpath-results-buffer-name :side bottom :ttl nil :height 0.3 :quit t :select nil :priority 100)
                            (,jg-xml-xmllint-shell-buffer-name :side right  :ttl nil :width  0.3 :quit t :select t   :priority 100)
                            )
                          )


  )

;;-- end popup

;;-- fold spec
(after! jg-fold-specs
  (setq jg-web-fold-spec `((web-mode)
                            :open-all   nil
                            :close-all  nil
                            :toggle     web-mode-fold-or-unfold
                            :open       nil
                            :open-rec   nil
                            :close      nil))
  (push jg-web-fold-spec evil-fold-list)
  )
;;-- end fold spec
