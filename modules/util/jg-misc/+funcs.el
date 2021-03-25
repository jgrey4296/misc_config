;;; util/jg-misc/+funcs.el -*- lexical-binding: t; -*-


(defun +jg-misc-undo-tree ()
  (interactive)
  (if (not undo-tree-mode)
      (undo-tree-mode))
   (undo-tree-visualize)

  )
