;; bibtex

(defhydra jg-org-ref-bibtex-hydra (:color blue)
  "
_p_: Open pdf     _y_: Copy key               _n_: New entry            _w_: WOS
_b_: Open url     _f_: Copy formatted entry   _o_: Copy entry           _c_: WOS citing
_r_: Refile entry _k_: Add keywords           _d_: delete entry         _a_: WOS related
_e_: Email entry  _K_: Edit keywords          _L_: clean entry          _P_: Pubmed
_U_: Update entry _N_: New entry              _R_: Crossref             _g_: Google Scholar
_s_: Sort entry   _a_: Remove nonascii        _h_: helm-bibtex          _q_: quit
_u_: Update field _F_: file funcs             _A_: Assoc pdf with entry
_N_: Open notes                               _T_: Title case
                                              _S_: Sentence case
"
  ("p" #'org-ref-open-bibtex-pdf)
  ("P" #'org-ref-bibtex-pubmed)
  ("w" #'org-ref-bibtex-wos)
  ("c" #'org-ref-bibtex-wos-citing)
  ("a" #'org-ref-bibtex-wos-related)
  ("R" #'org-ref-bibtex-crossref)
  ("g" #'org-ref-bibtex-google-scholar)
  ("n" #'org-ref-bibtex-new-entry/body)
  ("N" #'org-ref-open-bibtex-notes)
  ("o" (lambda ()
	 (interactive)
	 (bibtex-copy-entry-as-kill)
	 (message "Use %s to paste the entry"
		  (substitute-command-keys (format "\\[bibtex-yank]")))))
  ("d" #'bibtex-kill-entry)
  ("L" #'org-ref-clean-bibtex-entry)
  ("y" (save-excursion
	 (bibtex-beginning-of-entry)
	 (when (looking-at bibtex-entry-maybe-empty-head)
	   (kill-new (bibtex-key-in-head)))))
  ("f" (progn
	 (bibtex-beginning-of-entry)
	 (kill-new
	  (org-ref-format-entry
	   (cdr (assoc "=key=" (bibtex-parse-entry t)))))))
  ("k" #'helm-tag-bibtex-entry)
  ("K" (lambda ()
         (interactive)
         (org-ref-set-bibtex-keywords
          (read-string "Keywords: "
                       (bibtex-autokey-get-field "keywords"))
          t)))
  ("b" #'org-ref-open-in-browser)
  ("r" (lambda ()
	 (interactive)
         (bibtex-beginning-of-entry)
         (bibtex-kill-entry)
         (find-file (completing-read
                     "Bibtex file: "
                     (f-entries "." (lambda (f) (f-ext? f "bib")))))
         (goto-char (point-max))
         (bibtex-yank)
         (save-buffer)
         (kill-buffer)))
  ("e" #'org-ref-email-bibtex-entry)
  ("U" (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi)))
  ("u" #'doi-utils-update-field)
  ("F" #'org-ref-bibtex-file/body)
  ("h" #'helm-bibtex)
  ("A" #'org-ref-bibtex-assoc-pdf-with-entry)
  ("a" #'org-ref-replace-nonascii)
  ("s" #'org-ref-sort-bibtex-entry)
  ("T" #'org-ref-title-case-article)
  ("S" #'org-ref-sentence-case-article)
  ("q" nil))


(defun jg-tag-build-bibtex-list ()
  "Build a list of all bibtex files to use for bibtex-helm "
  (setq bibtex-completion-bibliography (directory-files jg-tag-loc-bibtex 't "\.bib$")))
(defun jg-tag-split-tags (x)
  (split-string x "," t "+")
  )

(defun jg-tag-bibtex-set-tags (x)
  " Set tags in bibtex entries "
  (let* ((visual-candidates (helm-marked-candidates))
         (actual-candidates (mapcar (lambda (x) (cadr (assoc x jg-tag-candidates-names))) visual-candidates))
         (prior-point 1)
         (end-pos jg-tag-marker)
         (current-tags '())
         (tag-regexp "\\(OPT\\)?tags")
         (has-real-tags-field nil)
         (add-func (lambda (candidate)
                     (if (not (-contains? current-tags candidate))
                         (progn
                           (push candidate current-tags)
                           (puthash candidate 1 jg-tag-global-tags))
                       (progn
                         (setq current-tags (remove candidate current-tags))
                         (puthash candidate (- (gethash candidate jg-tag-global-tags) 1) jg-tag-global-tags))
                       )))
         )
    (save-excursion
      (setq prior-point (- (point) 1))
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (progn
          (setq current-tags (jg-tag-split-tags (bibtex-autokey-get-field tag-regexp))
                prior-point (point)
                has-real-tags-field (not (string-empty-p (bibtex-autokey-get-field "tags")))
                )
          (mapc add-func actual-candidates)
          (bibtex-set-field (if has-real-tags-field "tags" "OPTtags")
                            (string-join current-tags ","))
          ;;(org-ref-bibtex-next-entry)
          (evil-forward-section-begin)
          )))
    )
)
(defun jg-tag-bibtex-set-new-tag (x)
  "A Fallback function to set tags of bibtex entries "
  (save-excursion
    (let ((prior-point (- (point) 1))
          (end-pos jg-tag-marker)
          (stripped_tags (jg-tag-split-tags (jg-tag-strip_spaces x)))
          (tag-regexp "\\(OPT\\)?tags")
          )
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (setq prior-point (point))
        (let* ((current-tags (jg-tag-split-tags (bibtex-autokey-get-field tag-regexp)))
               (filtered-tags (-filter (lambda (x) (not (-contains? current-tags x))) stripped_tags))
               (total-tags (-concat current-tags filtered-tags))
               (has-real-tags-field (not (string-empty-p (bibtex-autokey-get-field "tags"))))
               )
          (bibtex-set-field (if has-real-tags-field "tags" "OPTtags")
                            (string-join total-tags ","))
          (mapc (lambda (x) (puthash x 1 jg-tag-global-tags)) filtered-tags)
          ;;(org-ref-bibtex-next-entry)
          (evil-forward-section-begin)
          ))))
  )
(defun jg-tag-unify-pdf-locations-in-file (name)
  "Change all pdf locations in bibtex file to relative,
ensuring they work across machines "
  (message "Unifying Locations in %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (goto-char (point-min))
    (while (re-search-forward "file[[:digit:]]* ?= *{\\(.+mega\\)/\\(.+pdflibrary\\)?" nil t)
      (replace-match "~/Mega" nil nil nil 1)
      (if (eq 6 (length (match-data)))
          (replace-match "pdflibrary" t nil nil 2))
      )
    (write-file name)
    )
  )
(defun jg-tag-unify-pdf-locations ()
  "Unify bibtex pdf paths of marked files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-unify-pdf-locations-in-file files)
    )
  )

