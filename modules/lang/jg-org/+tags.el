;;; lang/jg-org/+tags.el -*- lexical-binding: t; -*-
(defun +jg-org-setup-tags-hook ()
  (+jg-tag-add-mode-handler 'org-mode
                            #'+jg-org-set-tags
                            #'+jg-org-set-new-tag
                            #'org-get-tags
                            )
  )

(defun +jg-org-set-tags (x)
  " Improved action to add and remove tags Toggle Selected Tags
Can operate on regions of headings "
  (let* ((actual-candidates (mapcar 'car (helm-marked-candidates)))
         (prior-point 1)
         (end-pos jg-tag-marker)
         (heading-re (if (org-property-values "TWITTER-BUFFER")
                         "^\*\* Thread"
                       "^\*"
                       )))

    (cond ((eq evil-state 'normal)
           (save-excursion
             (if (or (looking-at heading-re) (re-search-backward heading-re nil t))
                 (+jg-org-integrate-tags actual-candidates))
             ))
          ((eq evil-state 'visual)
           (save-excursion
             (setq prior-point (- (point) 1))
             (re-search-forward heading-re end-pos t)
             (while (and (/= prior-point (point)) (< (point) end-pos))
               (progn (setq prior-point (point))
                      (+jg-org-integrate-tags actual-candidates)
                      (org-forward-heading-same-level 1)))))
          (t (message "Unknown Tagging State")))))


(defun +jg-org-set-new-tag (x)
  "Utility to set a new tag for an org heading"
  (let ((prior-point (- (point) 1))
        (end-pos jg-tag-marker)
        (stripped-tag (+jg-text-strip-spaces x))
        (heading-re (if (org-property-values "TWITTER-BUFFER")
                        "^\*\* Thread"
                      "^\*"
                      )))

      (cond ((eq evil-state 'normal)
             (save-excursion
               (if (or (looking-at heading-re) (re-search-backward heading-re nil t))
                   (+jg-org-integrate-tags (list stripped-tag)))
               ))
            ((eq evil-state 'visual)
             (save-excursion
               (goto-char prior-point)
               (re-search-forward heading-re nil t)
               (while (and (/= prior-point (point)) (< (point) end-pos))
                 (setq prior-point (point))
                 (+jg-org-integrate-tags (list stripped-tag))
                 (org-forward-heading-same-level 1))))
            (t (message "Unknown Tagging State")))))


(defun +jg-org-integrate-tags (xs)
  (let ((current-tags (org-get-tags nil t)))
    (loop for tag in xs do
          (if (not (-contains? current-tags tag))
              (push tag current-tags)
            (setq current-tags (remove tag current-tags))
            )
          )
    (org-set-tags nil)
    (org-set-tags (sort current-tags #'string-lessp))
    )
)
