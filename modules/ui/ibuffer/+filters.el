;;; filters.el -*- lexical-binding: t; -*-

(define-ibuffer-filter unsaved-buffers
    "Filter for workspace buffers"
  (:description "unsaved buffers")
  (buffer-modified-p buf)
  )

(define-ibuffer-filter agenda-buffers
    "Filter for agendas"
  (:description "agenda buffers")
  (with-current-buffer buf jg-org-startup-agenda)
  )
