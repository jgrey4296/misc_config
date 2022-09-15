;;; +vars.el -*- lexical-binding: t; -*-

(setq jg-xml-xpath-command-string "xmllint --pretty 2 --htmlout --xpath %s %s"
      jg-xml-xpath-results-buffer-name  "*xpath result*"
      jg-xml-xmllint-shell-buffer-name "*xmllint*"
      )


(after! 'jg-popup-init
  (+jg-ui-popup-add-rules 'nxml
                          '((jg-xml-xpath-results-buffer-name :side bottom :ttl nil :height 0.3 :quit t :select t :priority 100)
                            (jg-xml-xmlint-shell-buffer-name  :side right  :ttl nil :height 0.3 :quit t :select t :priority 100)
                            )
                          )


  )
