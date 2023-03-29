;;; timeline-tags.el -*- lexical-binding: t; -*-
;; Handlers for jg-tags

(eval-after-load  'tagging-minor-mode
  (tagging-minor-mode-add-spec  'timeline-mode
                            '((:set +timeline-set-tags)
                              (:new +timeline-set-new-tag)
                              (:get +timeline-get-tags)
                              )
                            )
  )

(defun +timeline-set-tags (x))
(defun +timeline-set-new-tag (x))
(defun +timeline-get-tags () )

(provide 'timeline-tags)
