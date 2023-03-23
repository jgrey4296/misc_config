;;; +funcs.el -*- lexical-binding: t; -*-

;; Template for registering and applying ui settings

;;-- evil-fold
(defvar jg-fold-spec-table       (make-hash-table))
(defvar jg-fold-spec-sort-fn     (lambda (a b) (< (car a) (car b))))
(defvar jg-fold-spec-extract-fn  (lambda (a) (cdr a)))

;;;###autodef
(defun +jg-fold-add-spec (sym rules &optional priority override)
  "Register a fold spec for evil-fold-lit "
  (when (or (not (gethash sym jg-fold-spec-table)) override)
    (puthash sym (cons (or priority 0) rules) jg-fold-spec-table)
    )
  )
;;;###autodef
(defun +jg-fold-reapply-specs ()
  " Apply specs to the target "
  (interactive)
  (let ((vals (hash-table-values jg-fold-spec-table)))
    (setq evil-fold-list (mapcar jg-fold-spec-extract-fn
                                 (sort vals jg-fold-spec-sort-fn)))
  )
)
;;-- end evil-fold

;;-- hide-show
;; Template for registering and applying ui settings
(defvar jg-fold-hideshow-spec-table       (make-hash-table))
(defvar jg-fold-hideshow-spec-sort-fn     (lambda (a b) (< (car a) (car b))))
(defvar jg-fold-hideshow-spec-extract-fn  (lambda (a) (cdr a)))

;;;###autodef
(defun +jg-fold-hideshow-add-spec (sym rules &optional priority override)
  "Register a fold-hideshow spec for evil-fold-hideshow-lit "
  (when (or (not (gethash sym jg-fold-hideshow-spec-table)) override)
    (puthash sym (cons (or priority 0) rules) jg-fold-hideshow-spec-table)
    )
  )
;;;###autodef
(defun +jg-fold-hideshow-reapply-specs ()
  " Apply specs to the target "
  (interactive)
  (let ((vals (hash-table-values jg-fold-hideshow-spec-table)))
    (setq hs-special-modes-alist
          (apply 'append (mapcar jg-fold-hideshow-spec-extract-fn
                                 (sort vals jg-fold-hideshow-spec-sort-fn))))
    )
  )

;;-- end hide-show
