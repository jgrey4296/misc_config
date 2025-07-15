;;; pdf-track.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python-pdbtrack-silence (block)
  (if (and (not (string-match py-pdbtrack-stack-entry-regexp block))
           ;; pydb integration still to be done
           ;; (not (string-match py-pydbtrack-stack-entry-regexp block))
	   )
      (prog1 "Traceback cue not found")
    (let* ((remote-prefix (or (file-remote-p default-directory) ""))
           (filename (concat remote-prefix
                             (match-string
                              py-pdbtrack-marker-regexp-file-group block)))
           (lineno (string-to-number (match-string
                                      py-pdbtrack-marker-regexp-line-group
                                      block)))
           (funcname (match-string py-pdbtrack-marker-regexp-funcname-group
                                   block))
           funcbuffer)

      (cond ((string= filename "")
             (format "(Skipping empty filename)"))

            ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((file-exists-p (py--pdbtrack-map-filename filename))
             (list lineno (find-file-noselect (py--pdbtrack-map-filename filename))))

            ((setq funcbuffer (py--pdbtrack-grub-for-buffer funcname))
             (if (string-match "/Script (Python)$" filename)
                 ;; Add in number of lines for leading '##' comments:
                 (setq lineno
                       (+ lineno
                          (save-excursion
                            (with-current-buffer funcbuffer
			      (count-lines
			       (point-min)
			       (max (point-min)
				    (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
						  (buffer-substring (point-min)
								    (point-max))))))))))
             (list lineno funcbuffer))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s(), %s" funcname filename)))))
  )

;;;###autoload
(defun +jg-python-py--pdbtrack-track-stack-file (text)
  "Show the file indicated by the pdb stack entry line, in a separate window.

Activity is disabled if the buffer-local variable
‘py-pdbtrack-do-tracking-p’ is nil.

We depend on the pdb input prompt matching ‘py-pdbtrack-input-prompt’
at the beginning of the line.

If the traceback target file path is invalid, we look for the most
recently visited python-mode buffer which either has the name of the
current function \(or class) or which defines the function \(or
class).  This is to provide for remote scripts, eg, Zope's ‘Script
\(Python)’ - put a _copy_ of the script in a buffer named for the
script, and set to python-mode, and pdbtrack will find it.)"
  (let* ((origbuf (current-buffer))
         (currproc (get-buffer-process origbuf)))

    (if (not (and currproc py-pdbtrack-do-tracking-p))
        (py--pdbtrack-overlay-arrow nil)

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              py-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat py-pdbtrack-input-prompt "$") block))
            (py--pdbtrack-overlay-arrow nil)

          (setq target (py--pdbtrack-get-source-buffer block))

          (if (stringp target)
              (message "pdbtrack: %s" target)

            (setq target_lineno (car target)
                  target_buffer (cadr target)
                  target_fname (py--buffer-filename-remote-maybe target_buffer))
            (unless (get-buffer-window target_buffer)
              (switch-to-buffer-other-window target_buffer))
            (select-window (get-buffer-window target_buffer))
            (goto-char (point-min))
            (forward-line (1- target_lineno))
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (py--pdbtrack-overlay-arrow t)
            (recenter)
            (pop-to-buffer origbuf t))))
      )
    )
  )


;;;###autoload
(defun +jg-python-toggle-pdbtrack ()
  (interactive)
  (cond ((and (featurep 'python) (boundp 'python-pdbtrack-activate) python-pdbtrack-activate)
         ;; Deactivate
         (setq python-pdbtrack-activate nil)
         ;; (python-pdbtrack-unset-tracked-buffer)
         (message "PdbTrack: %s" python-pdbtrack-activate)
         )
        ((featurep 'python)
         (setq python-pdbtrack-activate t)
         ;; (python-pdbtrack-setup-tracking)
         (message "PdbTrack: %s" python-pdbtrack-activate)
         )
        ((featurep 'python-mode)
         (py-pdbtrack-toggle-stack-tracking)
         )
        )
  )

;;;###autoload
(defun +jg-python-pdbtrack-unset-fix-a ()
  (when (and (buffer-live-p python-pdbtrack-tracked-buffer)
             (buffer-local-boundp 'overlay-arrow-positioin python-pdbtrack-tracked-buffer))
    (with-current-buffer python-pdbtrack-tracked-buffer
      (set-marker overlay-arrow-position nil)))
  (setq python-pdbtrack-tracked-buffer nil)
  )
