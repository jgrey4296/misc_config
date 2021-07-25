;;; timeline-tags.el -*- lexical-binding: t; -*-
;; Handlers for jg-tags

(defun +timeline-setup-tags-hook ()
  ;; TODO wrap this in a guard
  (+jg-tag-add-mode-handler 'timeline-mode
                            #'+timeline-set-tags
                            #'+timeline-set-new-tag
                            #'+timeline-get-tags
                            )
  )

(defun +timeline-set-tags (x))
(defun +timeline-set-new-tag (x))
(defun +timeline-get-tags () )

(provide 'timeline-tags)
