;;; +advice.el -*- lexical-binding: t; -*-

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
(advice-add 'py--pdbtrack-get-source-buffer :override #'+jg-python-pdbtrack-silence)

;;;###autoload
(defun +jg-python-shell-calculate-command (&optional filepath)
  "Calculate the string used to execute the inferior Python process.
Adds in a few extra options like dev mode control,
a custom pycache location,
and adding extra pythonpath locations as the pre-args
"
  ;; `python-shell-make-comint' expects to be able to
  ;; `split-string-and-unquote' the result of this function.
  (s-join " "
          (--remove (not it)
                    (list
                     (combine-and-quote-strings (list python-shell-interpreter))
                     python-shell-interpreter-args
                     (if jg-python-dev-mode jg-python-dev-cmd)
                     ;; (format jg-python-pycache-cmd (f-canonical jg-python-pycache-loc))
                     (or filepath python-shell-interpreter-path-args)
                     ;; "--dir" (doom-project-root)
                     )
                    )
          )
    )

;;;###autoload
(advice-add 'python-shell-calculate-command :override #'+jg-python-shell-calculate-command)

;;;###autoload
(defun +jg-python-conda-get-path-prefix (env-dir)
  "Get a platform-specific path string to utilize the conda env in ENV-DIR.
It's platform specific in that it uses the platform's native path separator."
  (let* ((conda-anaconda-home-tmp conda-anaconda-home)
         (conda-executable-path
          (concat (file-name-as-directory conda-anaconda-home-tmp)
                  (file-name-as-directory conda-env-executables-dir)
                  "conda"))
         (base-command jg-conda-activate-cmd)
         (command (format base-command env-dir))
         (result
          (with-output-to-string
            (with-current-buffer standard-output
              (unless (= 0 (process-file shell-file-name nil '(t nil) nil shell-command-switch command))
                (error (format "Error: executing command \"%s\" produced error code %d" command return-code)))
              ))))
    (s-trim result))
  )

;;;###autoload
(advice-add 'conda--get-path-prefix :override #'+jg-python-conda-get-path-prefix)