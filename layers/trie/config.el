;; trie config.el
;; loaded fourth

(defgroup trie-modes '() "Customization group for trie-related modes")

;;Variables for the active ide
(setq trie/trie-ide-is-running nil
      trie/python-process nil
      trie/window-configuration nil
      trie/ide-data-loc nil
      trie/ide-pipeline-spec-buffer nil
      )


;;Defaults
(setq-default
 trie/inputs-buffer-name "*Rule Inputs*"
 trie/outputs-buffer-name "*Rule Outputs*"
 trie/working-group-buffer-name "*Rule Working Group*"
 trie/logging-buffer-name "*Rule Logs*"
 trie/working-group-buffer-headings '("Defeaters"
                                      "Interferers"
                                      "Alternatives"
                                      "Equal Depth"
                                      "Relevant Types"
                                      "Meta"
                                      "Layer Stats"
                                      "Tests")
 trie/data-loc-subdirs '("rules"
                         "types"
                         "crosscuts"
                         "patterns"
                         "tests")

)

(spacemacs|define-transient-state trie-help-hydra
      :title "Transient State for Help in Rule IDE"
      :doc (concat "
   | General           ^^|
   |-------------------^^+
   | [_q_] Quit          |
   |                   ^^|
   |                   ^^|
   |                   ^^|
  ")
      :bindings
      ("q" nil :exit t)
      )
