;;; tags.el -*- lexical-binding: t; -*-
(require 'doi-utils)

(defun +jg-bibtex-split-tags (x)
  (split-string x "," t "+")
  )

;;;###autoload
(defun +jg-bibtex-set-tags (x)
  " Set tags in bibtex entries "
  (let* ((actual-candidates (mapcar 'car (helm-marked-candidates)))
         (prior-point 1)
         (end-pos (max tagging-minor-mode-marker (save-excursion
                                                   (bibtex-end-of-entry)
                                                   (point))))
         (current-tags '())
         (has-real-tags-field nil)
         (add-func (lambda (candidate)
                     (if (not (-contains? current-tags candidate))
                         (progn
                           (push candidate current-tags)
                           (puthash candidate 1 tagging-minor-mode-global-tags))
                       (progn
                         (setq current-tags (remove candidate current-tags))
                         (puthash candidate (- (gethash candidate tagging-minor-mode-global-tags) 1) tagging-minor-mode-global-tags))
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

;;;###autoload
(defun +jg-bibtex-set-new-tag (x)
  "A Fallback function to set tags of bibtex entries "
  (save-excursion
    (let ((prior-point (- (point) 1))
          (end-pos (max tagging-minor-mode-marker (save-excursion
                                                    (bibtex-end-of-entry)
                                                    (point))))
          (stripped_tags (+jg-bibtex-split-tags (tagging-minor-mode--trim-input x)))
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
          (mapc (lambda (x) (puthash x 1 tagging-minor-mode-global-tags)) filtered-tags)
          ;;(org-ref-bibtex-next-entry)
          (evil-forward-section-begin)
          ))))
  )

;;;###autoload
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
