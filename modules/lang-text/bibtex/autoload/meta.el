;;; meta.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header
(defconst jg-bibtex-url-meta--name "* Url Metadata*")
(defconst jg-bibtex-url-meta--re (rx (| "description"
                                        "author"
                                        "tag"
                                        "publish"
                                        "date"
                                        "keywords"
                                        "doi"
                                        "byline"
                                        )
                                    ))
(defconst jg-bibtex-url-meta--user-agent "Mozilla/4.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36")

(defun jg-bibtex-url-meta--cb (status &rest args )
  "called with current buffer as the response"
  (re-search-forward "<html")
  (beginning-of-line)
  (let* ((dom  (libxml-parse-html-region (point) (point-max)))
         (selected (--> dom
                        (dom-by-tag it 'head)
                        (dom-search it (lambda (node)
                                         (pcase node
                                           (`(title . ,_) t)
                                           (`(meta ((property . ,_) ,_)) t)
                                           ((and `(meta ((name . ,x) . ,_))
                                                 (guard (s-matches? jg-bibtex-url-meta--re x)))
                                            t)
                                           (`(meta ,_) nil)
                                           (`(head . ,_) nil)
                                           (_ nil)
                                           )))
                        (apply #'dom-node 'selected nil it)
                   ))
         )
    ;; (erase-buffer)
    (with-current-buffer (get-buffer-create jg-bibtex-url-meta--name)
      (erase-buffer)
      (dom-pp selected)
      (goto-char (point-min))
      )
    (display-buffer jg-bibtex-url-meta--name 'display-buffer-at-bottom)
    )
  )

;;;###autoload
(defun jg-bibtex-url-meta ()
  (interactive)
  (let ((url-user-agent jg-bibtex-url-meta--user-agent)
        (url (save-excursion
               (bibtex-beginning-of-entry)
               (bibtex-autokey-get-field "url")
               ))
        )
    (url-retrieve url #'jg-bibtex-url-meta--cb)
    )
  )

;;;###autoload
(defun jg-bibtex-url-raw ()
  (interactive)
  (let* ((url-user-agent jg-bibtex-url-meta--user-agent)
         (url (save-excursion
                (bibtex-beginning-of-entry)
                (bibtex-autokey-get-field "url")
                ))
        )
    (display-buffer (url-retrieve-synchronously url t t))
    )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    March 21, 2025
;; Modified:   March 21, 2025
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
;;; meta.el ends here
