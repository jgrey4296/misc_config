;;; +fold.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-lsp-toggle-widget-on-line ()
  (interactive)
  (message "Pressing Widget")
  (widget-button-press
   (or (link-hint--next-widget
        (line-end-position)) (point)))
  )
