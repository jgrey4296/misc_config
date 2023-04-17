;;; +hooks.el -*- lexical-binding: t; -*-
;; Template for registering and applying ui settings
;;-- lookup
(defvar jg-browse-lookup-spec-table       (make-hash-table))
(defvar jg-browse-lookup-spec-sort-fn     (lambda (a b) (< (car a) (car b))))
(defvar jg-browse-lookup-spec-extract-fn  (lambda (a) (cdr a)))

;;;###autodef
(defun +jg-browse-add-lookup-spec (sym rules &optional priority override)
  "Register a browse spec for +lookup. rules are tuples (name url) "
  (when (or (not (gethash sym jg-browse-lookup-spec-table)) override)
    (puthash sym (cons (or priority 0) rules) jg-browse-lookup-spec-table)
    )
  )

;;;###autodef
(defun +jg-browse-reapply-lookup-specs ()
  " Apply specs to the target "
  (interactive)
  (let ((vals (hash-table-values jg-browse-lookup-spec-table)))
    (setq +lookup-provider-url-alist
          (apply 'append (mapcar jg-browse-lookup-spec-extract-fn
                                 (sort vals jg-browse-lookup-spec-sort-fn)))
          ))
  )

;;-- end lookup

;;-- browse
(defvar jg-browse-handler-spec-table       (make-hash-table))
(defvar jg-browse-handler-spec-sort-fn     (lambda (a b) (< (car a) (car b))))
(defvar jg-browse-handler-spec-extract-fn  (lambda (a) (cdr a)))

;;;###autodef
(defun +jg-browse-add-handler-spec (sym rules &optional priority override)
  "Register a browse spec for evil-browse-lit "
  (when (or (not (gethash sym jg-browse-handler-spec-table)) override)
    (puthash sym (cons (or priority 0) rules) jg-browse-handler-spec-table)
    )
  )

;;;###autodef
(defun +jg-browse-reapply-handler-specs ()
  " Apply specs to the target "
  (interactive)
  (let ((vals (hash-table-values jg-browse-handler-spec-table)))
    (setq browse-url-handlers
          (apply 'append
                 (mapcar jg-browse-handler-spec-extract-fn
                         (sort vals jg-browse-handler-spec-sort-fn))))
    )
  )

;;-- end browse
