;;; timeline-tags.el -*- lexical-binding: t; -*-
;; Handlers for jg-tags

(spec-handling-add! tagging
                    (timeline-mode
                     :set +timeline-set-tags
                     :new +timeline-set-new-tag
                     :get +timeline-get-tags
                     )
                    )

(defun +timeline-set-tags (x))
(defun +timeline-set-new-tag (x))
(defun +timeline-get-tags () )

(provide 'timeline-tags)
