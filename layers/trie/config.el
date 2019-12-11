;; trie config.el
;; loaded fourth

(defgroup trie-modes '() "Customization group for trie-related modes")

(setq trie/trie-ide-is-running nil
      trie/python-process nil
      trie/window-configuration nil
      trie/ide-data-loc nil
      )


(setq-default
 trie/inputs-buffer-name "*Rule Inputs*"
 trie/outputs-buffer-name "*Rule Outputs*"
 trie/working-group-buffer-name "*Rule Working Group*"
 trie/logging-buffer-name "*Rule Logs*"

 )
