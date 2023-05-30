;;; domain-specific/bibtex/+hydra.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+jg-bibtex-hydra/body "hydra" nil t)
(defhydra +jg-bibtex-hydra (:color blue)
  "
_w_: WOS
_c_: WOS citing
_a_: WOS related
_R_: Crossref
_U_: Update from doi
"
  ("w" #'org-ref-bibtex-wos)
  ("c" #'org-ref-bibtex-wos-citing)
  ("a" #'org-ref-bibtex-wos-related)
  ("R" #'org-ref-bibtex-crossref)
  ("U" (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi)))
  ("q" nil))
