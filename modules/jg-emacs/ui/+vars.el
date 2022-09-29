;;; editor/window-control/+vars.el -*- lexical-binding: t; -*-
(defvar jg-popup-display-rules (make-hash-table))
(defvar jg-popup-display-flattened nil)

(setq-default window-control-popup-persist-default '(:side bottom
                                                     :height 0.3
                                                     :quit t
                                                     :select nil
                                                     :modeline t
                                                     :ttl nil)
              highlight-parentheses-delay      0.3
              undo-tree-visualizer-diff        t
              undo-tree-auto-save-history      t
              undo-tree-enable-undo-in-region  t
              jg-ui-default-face-gen-palette-dir "/Volumes/documents/github/writing/resources/palettes/"
              display-line-numbers t
              )
