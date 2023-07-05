;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +latex-fold-snippet-contents-h ()
  (add-hook! 'yas-after-exit-snippet-hook :local
    (when (and yas-snippet-beg yas-snippet-end)
      (TeX-fold-region yas-snippet-beg yas-snippet-end))))

;;;###autoload
(defun +latex-fold-set-variable-pitch-h ()
      "Fix folded things invariably getting fixed pitch when using mixed-pitch.
Math faces should stay fixed by the mixed-pitch blacklist, this is mostly for
\\section etc."
      (when mixed-pitch-mode
        ;; Adding to this list makes mixed-pitch clean the face remaps after us
        (add-to-list 'mixed-pitch-fixed-cookie
                     (face-remap-add-relative
                      'TeX-fold-folded-face
                      :family (face-attribute 'variable-pitch :family)
                      :height (face-attribute 'variable-pitch :height))))
      )


;;;###autoload
(defun +latex-preview-scale-fn ()
  (* (/ 10.0 (preview-document-pt)) preview-scale)
)
