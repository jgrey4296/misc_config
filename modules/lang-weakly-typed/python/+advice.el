;;; +advice.el -*- lexical-binding: t; -*-

;; (define-advice python-info-dedenter-statement-p (:override ()
;;                                                  +jg-python-dedenter-rx-override)

;;   "Return point if current statement is a dedenter.
;; Sets `match-data' to the keyword that starts the dedenter
;; statement."
;;   (save-excursion
;;     (python-nav-beginning-of-statement)
;;     (when (and (not (python-syntax-context-type))
;;                (or (looking-at (python-rx dedenter))
;;                    (looking-at "\n\n+")
;;                    )
;;                )
;;       (point)
;;       )
;;     )
;;   )

;; (define-advice outline-next-heading (:before-until()
;;                                      +jg-python-override)
;;   (interactive)
;;   (if (and (bolp) (not (eobp))) (forward-char 1))
;;   (if (eq major-mode 'python-mode)
;;       (let* ((indent (python-indent--calculate-indentation))
;;              (total outline-regexp)) ;;(eval `(rx (or ,outline-regexp (** 1 ,indent " ") )))))
;;         (if (re-search-forward (concat "^\\(?:" total "\\)") nil 'move)
;;             (goto-char (match-beginning 0))
;;           )
;;         )
;;     )
;;   )

(define-advice py--pdbtrack-get-source-buffer (:override (block) +jg-python-pdftrack-silence)

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
