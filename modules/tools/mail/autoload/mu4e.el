;; mu4e.el -*- lexical-binding: t; -*-


;; Detect empty subjects, and give users an opotunity to fill something in
;;;###autoload
(defun +mu4e-check-for-subject ()
  "Check that a subject is present, and prompt for a subject if not."
  (save-excursion
    (goto-char (point-min))
    (search-forward "--text follows this line--")
    (re-search-backward "^Subject:") ; this should be present no matter what
    (let ((subject (string-trim (substring (thing-at-point 'line) 8))))
      (when (string-empty-p subject)
        (end-of-line)
        (insert (read-string "Subject (optional): "))
        (message "Sending...")))))

;;;###autoload
(add-hook 'message-send-hook #'+mu4e-check-for-subject)


;; The header view needs a certain amount of horizontal space to
;; actually show you all the information you want to see
;; so if the header view is entered from a narrow frame,
;; it's probably worth trying to expand it
;;;###autoload
(defun +mu4e-widen-frame-maybe ()
  "Expand the mu4e-headers containing frame's width to `+mu4e-min-header-frame-width'."
  (dolist (frame (frame-list))
    (when (and (string= (buffer-name (window-buffer (frame-selected-window frame)))
                        mu4e-headers-buffer-name)
               (< (frame-width) +mu4e-min-header-frame-width))
      (set-frame-width frame +mu4e-min-header-frame-width))))

;;;###autoload
(add-hook 'mu4e-headers-mode-hook #'+mu4e-widen-frame-maybe)


;; For mu4e > 1.6:
;;;###autoload
(defun +mu4e-view-select-attachment ()
  "Use completing-read to select a single attachment.
Acts like a singular `mu4e-view-save-attachments', without the saving."
  (if-let ((parts (delq nil (mapcar
                             (lambda (part)
                               (when (assoc "attachment" (cdr part))
                                 part))
                             (mu4e~view-gather-mime-parts))))
           (files (+mu4e-part-selectors parts)))
      (cdr (assoc (completing-read "Select attachment: " (mapcar #'car files)) files))
    (user-error (mu4e-format "No attached files found"))))

;;;###autoload
(defun +mu4e-view-open-attachment ()
  "Select an attachment, and open it."
  (interactive)
  (mu4e~view-open-file
   (mu4e~view-mime-part-to-temp-file (cdr (+mu4e-view-select-attachment)))))


;;;###autoload
(defun +mu4e-view-select-mime-part-action ()
  "Select a MIME part, and perform an action on it."
  (interactive)
  (let ((labeledparts (+mu4e-part-selectors (mu4e~view-gather-mime-parts))))
    (if labeledparts
        (mu4e-view-mime-part-action
         (cadr (assoc (completing-read "Select part: " (mapcar #'car labeledparts))
                      labeledparts)))
      (user-error (mu4e-format "No parts found")))))

;;;###autoload
(defun +mu4e-part-selectors (parts)
  "Generate selection strings for PARTS."
  (if parts
      (let (partinfo labeledparts maxfnamelen fnamefmt maxsizelen sizefmt)
        (dolist (part parts)
          (push (list :index (car part)
                      :mimetype (if (and (string= "text/plain" (caaddr part))
                                         (alist-get 'charset (cdaddr part)))
                                    (format "%s (%s)"
                                            (caaddr part)
                                            (alist-get 'charset (cdaddr part)))
                                  (caaddr part))
                      :type (car (nth 5 part))
                      :filename (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                      :size (file-size-human-readable (with-current-buffer (cadr part) (buffer-size)))
                      :part part)
                partinfo))
        (setq maxfnamelen (apply #'max 7 (mapcar (lambda (i) (length (plist-get i :filename))) partinfo))
              fnamefmt (format " %%-%ds  " maxfnamelen)
              maxsizelen (apply #'max (mapcar (lambda (i) (length (plist-get i :size))) partinfo))
              sizefmt (format "%%-%ds " maxsizelen))
        (dolist (pinfo partinfo)
          (push (cons (concat (propertize (format "%-2s " (plist-get pinfo :index)) 'face '(bold font-lock-type-face))
                              (when (featurep 'all-the-icons)
                                (all-the-icons-icon-for-file (or (plist-get pinfo :filename) "")))
                              (format fnamefmt (or (plist-get pinfo :filename)
                                                   (propertize (plist-get pinfo :type) 'face '(italic font-lock-doc-face))))
                              (format sizefmt (propertize (plist-get pinfo :size) 'face 'font-lock-builtin-face))
                              (propertize (plist-get pinfo :mimetype) 'face 'font-lock-constant-face))
                      (plist-get pinfo :part))
                labeledparts))
        labeledparts)))
