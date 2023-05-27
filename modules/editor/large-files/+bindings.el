;;; +bindings.el -*- lexical-binding: t; -*-

(setq vlf-mode-map (make-sparse-keymap))

(map! :mode vlf-mode
      "] A" 'vlf-next-batch-from-point
      "] a" 'vlf-next-batch
      "[ a" 'vlf-prev-batch
      "SPC a U v " 'vlf-set-batch-size
      )
