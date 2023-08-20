;;; filters.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'ibuffer-filter-by-unsaved-buffers "ui/ibuffer/autoload/filters" nil t)
(define-ibuffer-filter unsaved-buffers
    "Filter for workspace buffers"
  (:description "Get buffers that are unsaved")
  (buffer-modified-p buf)
  )
