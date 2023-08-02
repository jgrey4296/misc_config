;;; emacs/jg-tag/+funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-tag-open-random-untagged-twitter ()
  (interactive)
  (let* ((av-dirs (f-entries jg-tag-loc-twitter
                                   #'(lambda (x) (cond ((f-file? x) nil)
                                                  (t t)))))
         (chosen-dir (seq-random-elt av-dirs))
         (av-orgs (f-files chosen-dir #'(lambda (x) (f-ext? x "org"))))
         (chosen-org (seq-random-elt av-orgs))
        )
    (find-file chosen-org)
    )
  )

;;;###autoload
(defun +jg-tag-split-tags()
  (interactive)
  (goto-char (point-min))
  (let ((letter ?a)
        (end-letter (+ 1 ?z))
        (beg (point-min))
        (fst t)
        subs)
    (while (and (not (equal letter end-letter))
                (re-search-forward (format "^%s" (char-to-string letter)) nil nil))
      (setq subs (buffer-substring beg (- (point) 1)))
      (with-output-to-temp-buffer (if fst "misc.tags" (format "%s.tags" (char-to-string (- letter 1))))
        (princ subs)
        )
      (setq beg (- (point) 1)
            letter (+ letter 1)
            fst nil)
      )
    (setq subs (buffer-substring (- (point) 1) (point-max)))
    (with-output-to-temp-buffer "z.tags"
      (princ subs)
      )
    )
  )

;;;###autoload
(defun +jg-tag-save-helm-buffer ()
  (interactive)
  (let ((results (with-helm-buffer (buffer-string))))
    (helm-exit-and-execute-action
     #'(lambda (x)
         (with-temp-buffer-window "TestBuffer" 'display-buffer-pop-up-frame nil
           (princ results)
           )
         )
     )
    )
  )

;;;###autoload
(defun +jg-tag-file-display (candidates)
  (interactive)
  (let*((candidates (plist-get (car (helm-marked-candidates)) :files)))
    (with-temp-buffer-window "Helm Twitter Grep Results"
        'display-buffer-pop-up-window nil
      (mapcar (lambda (x) (princ x) (princ "\n")) candidates)
      )
    )
  )
