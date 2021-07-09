;;; emacs/jg-tag/+funcs.el -*- lexical-binding: t; -*-

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
