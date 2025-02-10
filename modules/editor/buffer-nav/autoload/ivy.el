;;; ivy.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun +ivy/jump-list (arg)
  "Go to an entry in evil's (or better-jumper's) jumplist."
  (interactive "p")
  (pcase arg
    (1 (counsel-evil-marks--with-state-normal))
    (4 (+jg-buffer-nav-jumplist--with-state-normal t))
    (_ (+jg-buffer-nav-jumplist--with-state-normal))
    )
  )

(with-state! 'normal #'+jg-buffer-nav-jumplist)

(defun +jg-buffer-nav-jumplist (&optional use-curr)
  (interactive)
  (ivy-read "Jumplist: "
            (jg-buffer-nav-filter-jumplist (when use-curr (current-buffer)))
            :sort nil
            :require-match t
            :action (lambda (cand)
                      (let* ((data (cdr cand))
                             (mark (car data))
                             (idx (cdr data))
                             )
                          (better-jumper--jump idx 0)
                        )
                      )
            :caller '+jg-buffer-nav-jumplist)
  )

(defun jg-buffer-nav-filter-jumplist (&optional curr)
  (let* ((ctx      (better-jumper--get-current-context))
         (jumps    (better-jumper-get-jumps ctx))
         (jumpring (better-jumper-jump-list-struct-ring jumps))
         (hash     (make-hash-table :test 'equal))
         )
    (cl-loop for mark being the elements of (ring-elements jumpring)
             with idx = -1
             do
             (cl-incf idx)
             and
             if (null mark)
             do 'skip
             else
             for parsed = (cl-destructuring-bind (path pt key) mark
                            (let ((buf (get-file-buffer path)))
                              (when (and buf (or (not curr) (eq buf curr))
                                         (not (gethash (cons path pt) hash))
                                         )
                                (puthash (cons path pt) t hash )
                                (with-current-buffer buf
                                  (save-excursion
                                    (goto-char pt)
                                    (cons (format "(%s) %s:%d: %s"
                                                  idx
                                                  (buffer-name)
                                                  (line-number-at-pos)
                                                  (string-trim-right (or (thing-at-point 'line) ""))
                                                  )
                                          (cons (point-marker) idx)
                                          ))))))
             if parsed
             collect parsed
             )
    )
  )


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 10, 2025
;; Modified:   February 10, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; ivy.el ends here
