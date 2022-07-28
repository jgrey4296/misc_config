;;; editor/window-control/+vars.el -*- lexical-binding: t; -*-
(defvar jg-popup-display-rules (make-hash-table)

(setq-default window-control-popup-persist-default '(:side bottom
                                                     :height 0.3
                                                     :quit t
                                                     :select nil
                                                     :modeline t
                                                     :ttl nil)

              undo-tree-visualizer-diff t
              undo-tree-auto-save-history t
              undo-tree-enable-undo-in-region t
              jg-ui-default-face-gen-palette-dir "/Volumes/documents/github/writing/resources/palettes/"
              )
