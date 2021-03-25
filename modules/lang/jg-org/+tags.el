;;; lang/jg-org/+tags.el -*- lexical-binding: t; -*-
(defun +jg-org-setup-tags-hook ()
  (+jg-tag-add-mode-handler 'org
                            '+jg-org-set-tags
                            '+jg-org-set-new-tag)
  )

(defun +jg-org-set-tags (x)
  """ Improved action to add and remove tags Toggle Selected Tags
Can operate on regions of headings """
  (let* ((visual-candidates (helm-marked-candidates))
         (actual-candidates (mapcar (lambda (x) (cadr (assoc x jg-tag-candidates-names))) visual-candidates))
         (prior-point 1)
         (end-pos jg-tag-marker)
         (current-tags '())
         (add-func (lambda (candidate)
                     (if (not (-contains? current-tags candidate))
                         (progn
                           (push candidate current-tags)
                           (puthash candidate 1 jg-tag-global-tags))
                       (progn
                         (setq current-tags (remove candidate current-tags))
                         (puthash candidate (- (gethash candidate jg-tag-global-tags) 1) jg-tag-global-tags))
                       ))))
    (save-excursion
      (setq prior-point (- (point) 1))
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (progn (setq current-tags (org-get-tags nil t)
                     prior-point (point))
               (mapc add-func actual-candidates)
               (org-set-tags current-tags)
               (org-forward-heading-same-level 1)
               )))))
(defun +jg-org-set-new-tag (x)
  "Utility to set a new tag for an org heading"
  (save-excursion
    (let ((prior-point (- (point) 1))
          (end-pos jg-tag-marker)
          (stripped_tag (+jg-text-strip-spaces x))
          )
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (setq prior-point (point))
        (let* ((current-tags (org-get-tags nil t)))
          (if (not (-contains? current-tags stripped_tag))
              (progn
                (push stripped_tag current-tags)
                (puthash stripped_tag 1 jg-tag-global-tags)))
          (org-set-tags current-tags)
          (org-forward-heading-same-level 1)
          )))))
