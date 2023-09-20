;; warnings.el -*- lexical-binding: t; -*-

(defvar jg-default-warning-interactive-fn #'+jg-default-warning-formatter-basic "Called with the warning buffer as current")

(defun +jg-default-warning-formatter-basic (type message level)
  (insert (format "Warning: %s : %s" type message))
  ;; (insert (format (nth 1 level-info) (format warning-type-format typename)) message)
  )

;;;###autoload
(defun +jg-default-display-warning-ad (type message &optional level buffer-name)
  ;; before-until. return t to use, nil to use default
  (unless (or noninteractive (eq type 'bytecomp))
    (unless level (setq level :warning))
    (let* ((typename (if (consp type) (car type) type))
	   (old (get-buffer (or buffer-name "*Warnings*")))
	   (buffer (or old (get-buffer-create (or buffer-name "*Warnings*"))))
           )
      (+jg-default-warning-formatter buffer old type level message)
      (+jg-default-warning-display buffer type level)
      t
      )
    )
  )

(defun +jg-default-warning-formatter (buffer old type level message)
  (with-current-buffer buffer
    (unless old ;; If we created the buffer, disable undo.
      (when (fboundp 'special-mode) (special-mode))  ; Undefined during bootstrap.
      (setq buffer-read-only t
            buffer-undo-list t)
      )
    (goto-char (point-max))
    (when (and warning-series (symbolp warning-series))
      (setq warning-series (prog1 (point-marker) (unless (eq warning-series t) (funcall warning-series)))))

    (let ((inhibit-read-only t)
          (level-info (assq (or level :warning) warning-levels))
	  (fill-prefix warning-fill-prefix)
	  (fill-column warning-fill-column)
          )
      (unless (bolp) (newline))
      (if (nth 2 level-info) (funcall (nth 2 level-info)))
      (funcall jg-default-warning-interactive-fn type message level)
      (when (and warning-fill-prefix (not (string-search "\n" message))) (fill-region start (point)))
      )
    )
  )

(defun +jg-default-warning-display (buffer type level)
  (cond ((< (warning-numeric-level level) (warning-numeric-level warning-minimum-level))
         nil)
        ((warning-suppress-p type warning-suppress-types)
         nil)
        (t
         (let ((window (display-buffer buffer)))
	   (when (and (markerp warning-series) (eq (marker-buffer warning-series) buffer))
	     (set-window-start window warning-series))
	   (sit-for 0)))
        )
  )

;;;###autoload
(advice-add 'display-warning :before-until #'+jg-default-display-warning-ad)
