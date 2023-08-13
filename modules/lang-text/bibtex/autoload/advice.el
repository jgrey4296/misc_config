;;; +advice.el -*- lexical-binding: t; -*-
(require 'f)
(require 's)

;;;###autoload
(defun +jg-build-bibliography ()
  "Build pdf of all bibtex entries, and open it."
  (interactive)
  (let* ((loc default-directory)
         (build-loc (f-join loc "tex"))
         (bibfile   (file-name-nondirectory (buffer-file-name)))
         (bib-base  (file-name-sans-extension bibfile))
         (target    (f-join build-loc bib-base))
         (pdffile   (concat bib-base ".pdf"))
         (texfile   (concat bib-base ".tex")))
    (if (not (f-exists? build-loc))
        (f-mkdir build-loc))
    (with-temp-buffer
      (insert-file-contents (expand-file-name jg-bibtex-loc-export-bib-file))
      (goto-char (point-min))
      (re-search-forward "%target")
      (replace-match (f-join loc bibfile))
      (write-file (f-join build-loc texfile))
      )
    (let ((default-directory build-loc))
      (shell-command (concat "pdflatex " (shell-quote-argument texfile)))
      (shell-command (concat "bibtex "  (shell-quote-argument bib-base)))
      (shell-command (concat "pdflatex " (shell-quote-argument texfile)))
      (shell-command (concat "pdflatex " (shell-quote-argument texfile)))
      (shell-command (concat jg-bibtex-open-pdf-cmd pdffile))
      )
    )
  )

;;;###autoload
(defun +jg-org-ref-version-override (f)
  (let ((kill-ring nil))
    (funcall f)
    )
  )

;;;###autoload
(defun +jg-bibtex-set-field (field value &optional nodelim)
  "Set FIELD to VALUE in bibtex file.  create field if it does not exist.
Optional argument NODELIM ignored to fit `bibtex-make-field` signature
Modified to avoid duplicate comma insertion. "
  (interactive "sfield: \nsvalue: ")
  (bibtex-beginning-of-entry)
  (let ((found))
    (if (setq found (bibtex-search-forward-field (concat field "\\b") t))
        ;; we found a field
        (progn
          (goto-char (car (cdr found)))
          (when value
            (bibtex-kill-field)
            (bibtex-make-field field nil nil nil)
            (backward-char)
            (insert value)))
      ;; make a new field
      (bibtex-beginning-of-entry)
      (forward-line) (beginning-of-line)
      ;; (bibtex-next-field nil)
      ;; (forward-char)
      (bibtex-make-field field t nil nil)
      (backward-char)
      (insert value))))

;;;###autoload
(defun +jg-bibtex-autokey-field-expand (fn &rest args)
  (if-let* ((first-arg (if (listp (car args)) (car args) (list (car args))))
            (matches (-any? (-partial #'s-matches? "file[[:digit:]]*") first-arg))
            (result (apply fn args))
            (not-empty (not (s-equals? "" result)))
            )
      (+jg-bibtex-file-expand result)
    (apply fn args)
    )
  )

;;;###autoload
(defun +jg-bibtex-init-no-file-watchers ()
  "Check that the files and directories specified by the user actually exist.
Also sets `bibtex-completion-display-formats-internal'."
  (+jg-bibtex-suppress-watchers)
  ;; Pre-calculate minimal widths needed by the format strings for
  ;; various entry types:
  (setq bibtex-completion-display-formats-internal
        (mapcar (lambda (format)
                  (let* ((format-string (cdr format))
                         (fields-width 0)
                         (string-width
                          (string-width
                           (s-format format-string
                                     (lambda (field)
                                       (setq fields-width
                                             (+ fields-width
                                                (string-to-number
                                                 (or (cadr (split-string field ":"))
                                                     ""))))
                                       "")))))
                    (-cons* (car format) format-string (+ fields-width string-width))))
                bibtex-completion-display-formats))
  )

;;;###autoload
(advice-add 'org-ref-build-full-bibliography :override #'+jg-build-bibliography)
;;;###autoload
(advice-add 'bibtex-autokey-get-field :around #'+jg-bibtex-autokey-field-expand)
;;;###autoload
(advice-add 'bibtex-set-field :override #'+jg-bibtex-set-field)
;;;###autoload
(advice-add 'org-ref-version :around #'+jg-org-ref-version-override)
;;;###autoload
(advice-add 'bibtex-completion-init :override #'+jg-bibtex-init-no-file-watchers)


;;; +advice.el ends here
