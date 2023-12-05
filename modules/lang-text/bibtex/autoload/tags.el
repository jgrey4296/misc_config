;;; tags.el -*- lexical-binding: t; -*-
(require 'doi-utils)
(require 'bibtex)

(defun +jg-bibtex-split-tags (x)
  (split-string x "," t "+")
  )

(defun +jg-bibtex--mod-tags-fn (current candidate)
  (cond ((-contains? current candidate)
         (puthash candidate (- (gethash candidate librarian-tagging-mode-global-tags) 1) librarian-tagging-mode-global-tags)
         (delete candidate current)
         )
        (t
         (puthash candidate 1 librarian-tagging-mode-global-tags)
         (cons candidate current)
         )
        )
  )

;;;###autoload
(defun +jg-bibtex-set-tags (x)
  " Set tags in bibtex entries "
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((actual-candidates (mapcar 'car (helm-marked-candidates)))
           (start-pos (point))
           )
      (goto-char librarian-tagging-mode-marker)
      (evil-forward-section-end)
      (while (> (point) start-pos)
        (bibtex-set-field "tags"
                          (string-join
                           (cl-reduce #'+jg-bibtex--mod-tags-fn actual-candidates
                                      :initial-value
                                      (+jg-bibtex-split-tags (bibtex-autokey-get-field '("tags" "OPTtags"))))
                           ","))
        (evil-backward-section-begin 2) ;; count 2. first to return to entry beginning, then to go beyond
        ))
    )
  )

;;;###autoload
(defun +jg-bibtex-set-new-tag (x)
  "A Fallback function to set tags of bibtex entries "
  (save-excursion
    (bibtex-beginning-of-entry)
    (let ((start-pos (point))
          (stripped_tags (+jg-bibtex-split-tags (librarian-tagging-mode--trim-input x)))
          )
      (goto-char librarian-tagging-mode-marker)
      (evil-forward-section-end)
      (while (>= (point) start-pos)
        (let* ((current-tags (+jg-bibtex-split-tags (bibtex-autokey-get-field '("tags" "OPTtags"))))
               (filtered-tags (-filter (lambda (x) (not (-contains? current-tags x))) stripped_tags))
               )
          (bibtex-set-field "tags" (string-join (append current-tags filtered-tags) ","))
          (mapc (lambda (x) (puthash x 1 librarian-tagging-mode-global-tags)) filtered-tags)
          (evil-backward-section-begin 2) ;; count 2. first to return to entry beginning, then to go beyond
          )))))

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
