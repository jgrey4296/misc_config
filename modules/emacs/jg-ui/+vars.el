;;; editor/window-control/+vars.el -*- lexical-binding: t; -*-
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

(push '("^\\*Async Shell Command\\*$" (display-buffer-no-window))
      display-buffer-alist)
