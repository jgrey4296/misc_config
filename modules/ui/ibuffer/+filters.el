;;; filters.el -*- lexical-binding: t; -*-

(define-ibuffer-filter unsaved-buffers
    "Filter for workspace buffers"
  (:description "unsaved buffers")
  (buffer-modified-p buf)
  )

(define-ibuffer-filter agenda-buffers
    "Filter for agendas"
  (:description "agenda buffers")
  (buffer-local-value 'jg-org-startup-agenda buf)
  )
