;;; util/+jg-bibtex/+hooks.el -*- lexical-binding: t; -*-

;; Clean bibtex hooks:
;; adapted from org-ref/org-ref-core.el: orcb-key-comma
;; For org-ref-clean-bibtex-entry-hook
(defun +jg-bibtex-fix-paths (pair)
  `(,(car pair)
    .
    ,(+jg-bibtex--recode-non-ascii (substring (cdr pair) 1 -1))))
(defun +jg-bibtex-set-pair (pair)
  (bibtex-set-field (car pair) (cdr pair))
  )
(defun +jg-bibtex--recode-non-ascii (x)
  ;; jg-bibtex-non-ascii-inverted
  ;; BUG can't handle {\\c{s}}
  (with-temp-buffer
    (insert x)
    (goto-char (point-min))
    (let (last-match replace-data)
      (while (re-search-forward "{.+?}" nil t)
        (setq replace-data (assoc (match-string 0) jg-bibtex-non-ascii-inverted))
        (if replace-data
            (replace-match (cdr replace-data) nil nil nil 0)
          )
        )
      (buffer-string)
      )
    )
  )
(defun +jg-bibtex-recode-files-hook ()
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (file-likes (-filter 'identity (mapcar #'+jg-bibtex--get-file-entries entry)))
         (fixed (mapcar #'+jg-bibtex-fix-paths file-likes))
         )
    (mapc #'+jg-bibtex-set-pair fixed)
    )
  )
(defun +jg-bibtex-font-lock-mod-hook ()
  (pushnew!
   bibtex-font-lock-keywords
   '(" title.+$" (0 '(:background "mediumpurple4")))
   '("\\(file\\).+?=" (1 '(:background "darkgoldenrod")))
   )
  )
