;;; domain-specific/acab-ide/+vars.el -*- lexical-binding: t; -*-

(defgroup trie-modes '() "Customization group for trie-related modes")

(defvar acab-ide/rule-helm-source nil
  "Main Helm Source for rule loading / finding ")
(defvar acab-ide/rule-helm-dummy-source)
(defvar acab-ide/type-helm-source nil
  "Main Helm Source for type loading / finding ")
(defvar acab-ide/type-helm-dummy-source)


;;Variables for the active ide
(setq acab-ide/trie-ide-is-running      nil
      acab-ide/python-process           nil
      acab-ide/window-configuration     nil
      acab-ide/ide-data-loc             nil
      acab-ide/ide-pipeline-spec-buffer nil
      )

(defcustom acab-ide/process-python-command
  "python3"
  "Name of the Python program to use")
(defcustom acab-ide/process-python-args
  ""
  "Arguments to python to run IDE server")

;;Defaults
(setq-default acab-ide/inputs-buffer-name         "*Rule Inputs*"
              acab-ide/outputs-buffer-name        "*Rule Outputs*"
              acab-ide/working-group-buffer-name  "*Rule Working Group*"
              acab-ide/logging-buffer-name        "*Rule Logs*"
              acab-ide/python-process-buffer-name "*Rule IDE*"

              acab-ide/working-group-buffer-headings '("Defeaters"
                                                       "Interferers"
                                                       "Alternatives"
                                                       "Equal Depth"
                                                       "Relevant Types"
                                                       "Meta"
                                                       "Layer Stats"
                                                       "Tests")
              acab-ide/data-loc-subdirs '("rules"
                                          "types"
                                          "crosscuts"
                                          "patterns"
                                          "tests")

              )
