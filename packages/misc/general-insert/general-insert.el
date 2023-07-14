;;; general-insert.el -*- lexical-binding: t; -*-
;; TODO handle name conflicts between modes

(defvar general-insert-location nil)

(defvar general-insert--cache     (make-hash-table :test 'equal))

(defvar general-insert--key-cache (make-hash-table :test 'equal))

(defvar general-insert-processors (make-hash-table :test 'equal))

(defvar-local general-insert-keys nil)

(defun general-insert--propertize (mode file)
  (let ((base (format "%-20s # %s" (f-base file) mode)))
    (set-text-properties 0 (length base) `(path ,file) base)
    base
    )
  )

(defun general-insert-clear-caches ()
  (interactive)
  (setq general-insert--cache (make-hash-table :test 'equal)
        general-insert--key-cache (make-hash-table :test 'equal)
        )
  (setq-local general-insert-keys nil)
  )

;;;###autoload
(define-minor-mode general-insert-minor-mode
  " "
  :init-value nil
  :lighter "general-insert"
  (setq-local general-insert-keys
              (cl-loop for mode in (append (parent-mode-list major-mode) '(fundamental-mode))
                       when (f-exists? (f-join general-insert-location (symbol-name mode)))
                       do
                       (unless (gethash mode general-insert--key-cache)
                         (puthash mode
                                  (mapcar (-partial #'general-insert--propertize (symbol-name mode)) (f-files (f-join general-insert-location (symbol-name mode))))
                                  general-insert--key-cache))
                       and
                       append
                       (gethash mode general-insert--key-cache)
                       )

            )
  )

(defun general-insert-default (x)
  (insert (car (split-string x "#" t " +")))
  )

;;;###autoload
(defun general-insert-call ()
  " trigger simple insertions "
  (interactive)
  (ivy-read "Insert: " general-insert-keys
            :require-match t
            :action #'general-insert-call-sub-ivy)

  )

(defun general-insert-call-sub-ivy (selected)
  " "
  (unless (gethash selected general-insert--cache)
    (puthash selected
             (general-insert--load-file
              (get-text-property 0 'path selected))
             general-insert--cache))

  (-when-let* ((vals (gethash selected general-insert--cache))
               (processor (or (gethash `(,major-mode ,selected) general-insert-processors) #'general-insert-default))
               )
      (ivy-read (format "%s " (car vals))
                (cdr vals)
                :action processor
                :require-match t
                )
    )
)

;;;###autoload
(defun general-insert-register-processor (mode key fn)
  (puthash (list mode key) fn general-insert-processors))

(defun general-insert--load-file (file)
  "read a (prompt . (items:list)) from the given file"
  (let (targets)
    (with-temp-buffer
      (insert-file-contents file)
      (setq targets (s-lines (buffer-substring-no-properties (point-min) (point-max))))
      )
    targets
    )
  )

(provide 'general-insert)
