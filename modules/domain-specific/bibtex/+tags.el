;;; domain-specific/bibtex/+tags.el -*- lexical-binding: t; -*-

(defun +jg-bibtex-tag-setup-hook ()
  (+jg-tag-add-mode-handler 'bibtex-mode
                            #'+jg-bibtex-set-tags
                            #'+jg-bibtex-set-new-tag
                            #'+jg-bibtex-get-tags
                            )

  )
(defun +jg-bibtex-split-tags (x)
  (split-string x "," t "+")
  )
(defun +jg-bibtex-set-tags (x)
  " Set tags in bibtex entries "
  (let* ((actual-candidates (mapcar 'car (helm-marked-candidates)))
         (prior-point 1)
         (end-pos jg-tag-marker)
         (current-tags '())
         (has-real-tags-field nil)
         (add-func (lambda (candidate)
                     (if (not (-contains? current-tags candidate))
                         (progn
                           (push candidate current-tags)
                           (puthash candidate 1 jg-tag-global-tags))
                       (progn
                         (setq current-tags (remove candidate current-tags))
                         (puthash candidate (- (gethash candidate jg-tag-global-tags) 1) jg-tag-global-tags))
                       )))
         )
    (save-excursion
      (setq prior-point (- (point) 1))
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (progn
          (setq has-real-tags-field (not (string-empty-p (bibtex-autokey-get-field "tags"))))
          (setq current-tags (+jg-bibtex-split-tags (bibtex-autokey-get-field
                                                     (if has-real-tags-field "tags"
                                                       "OPTtags")))
                prior-point (point))
          (mapc add-func actual-candidates)
          (bibtex-set-field "tags"
                            (string-join current-tags ","))
          ;;(org-ref-bibtex-next-entry)
          (evil-forward-section-begin)
          )))
    )
)
(defun +jg-bibtex-set-new-tag (x)
  "A Fallback function to set tags of bibtex entries "
  (save-excursion
    (let ((prior-point (- (point) 1))
          (end-pos jg-tag-marker)
          (stripped_tags (+jg-bibtex-split-tags (+jg-text-strip-spaces x)))
          )
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (setq prior-point (point))
        (let* ((has-real-tags-field (not (string-empty-p (bibtex-autokey-get-field "tags"))))
               (current-tags (+jg-bibtex-split-tags (bibtex-autokey-get-field
                                                     (if has-real-tags-field "tags" "OPTtags"))))
               (filtered-tags (-filter (lambda (x) (not (-contains? current-tags x))) stripped_tags))
               (total-tags (-concat current-tags filtered-tags))
               )
          (bibtex-set-field "tags"
                            (string-join total-tags ","))
          (mapc (lambda (x) (puthash x 1 jg-tag-global-tags)) filtered-tags)
          ;;(org-ref-bibtex-next-entry)
          (evil-forward-section-begin)
          ))))
  )
(defun +jg-bibtex-get-tags ()
  (let ((tags (bibtex-autokey-get-field "tags"))
        (opttags (bibtex-autokey-get-field "OPTtags")))
    (split-string (concat (bibtex-autokey-get-field "tags")
                          (bibtex-autokey-get-field "OPTtags"))
                  ","
                  t
                  "\s")
    )
)
