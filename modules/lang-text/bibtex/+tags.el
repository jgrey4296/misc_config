;;; tags.el -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'doi-utils)
(require 'bibtex)

(cl-defmethod librarian-set-tags ((mode (eql 'bibtex-mode)) add sub keep)
  " Set tags in bibtex entries "
  (let ((joined (string-join (cl-concatenate 'list add keep) ","))
        )
    (bibtex-beginning-of-entry)
    (bibtex-set-field "tags" joined)
    )
  )

(cl-defmethod librarian-set-new-tags  ((mode (eql 'bibtex-mode)) new)
  "A Fallback function to set tags of bibtex entries "
  (save-excursion
    (let ((joined (string-join new ",")))
      (bibtex-beginning-of-entry)
      (bibtex-set-field "tags" joined)
      )
    )
  )

(cl-defmethod librarian-get-tags ((mode (eql 'bibtex-mode)))
  (let ((tags (bibtex-autokey-get-field "tags"))
        (opttags (bibtex-autokey-get-field "OPTtags")))
    (split-string (concat (bibtex-autokey-get-field "tags")
                          (bibtex-autokey-get-field "OPTtags")
                          (bibtex-autokey-get-field "keywords"))
                  ","
                  t
                  "\s+")
    )
  )

(cl-defmethod librarian-backward-entry ((mode (eql 'bibtex-mode)))
  (bibtex-previous-entry)
  )
