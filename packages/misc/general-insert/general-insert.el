;;; general-insert.el -*- lexical-binding: t; -*-
;; TODO handle name conflicts between modes

(defvar general-insert-location nil)

(defvar general-insert--cache     (make-hash-table :test 'equal))

(defvar general-insert--key-cache (make-hash-table))

(defvar general-insert-processors (make-hash-table :test 'equal))

(defvar-local general-insert-keys nil)

(define-minor-mode general-insert-minor-mode
  " "
  :init-value nil
  :lighter "general-insert"
  (setq-local general-insert-keys
              (cl-loop for mode in (parent-mode-list major-mode)
                       when (f-exists? (f-join general-insert-location (symbol-name mode)))
                       do
                       (unless (gethash mode general-insert--key-cache)
                         (puthash mode
                                  (mapcar #'f-base (f-files (f-join general-insert-location (symbol-name mode))))
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
              (f-join general-insert-location (symbol-name major-mode) selected))
             general-insert--cache))
  (-when-let* ((vals (gethash selected general-insert--cache))
               (processor (gethash `(,major-mode ,selected) general-insert-processors #'general-insert-default))
               )
      (ivy-read (format "%s " (car vals))
                (cdr vals)
                :action (symbol-function processor)
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
