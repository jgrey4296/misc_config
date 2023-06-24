;; https://orgmode.org/worg/dev/org-export-reference.html
;; https://orgmode.org/worg/dev/org-element-api.html#attributes

(require 'cl-lib)
(require 'dash)
(require 's)

(require 'org)
(require 'ox)
(require 'ox-html)
(require 'ox-html-epub-vars)
(require 'ox-html-epub-utils)
(require 'ox-html-epub-transcoders)

;;-- backend definition

(defvar org-html-epub-menu nil)
  ;; '(?h "Export to HTML"
  ;;   ((?J "As Epub HTML buffer" org-html-epub-export-as-html)
  ;;    (?j "As Epub HTML file"   org-html-epub-export-to-html)
  ;;    (?O "As HTML file and open" (lambda (a s v b)
  ;;                                  (if a (org-html-epub-export-to-html t s v b)
  ;;       	                     (org-open-file (org-html-epub-export-to-html nil s v b))))))
  ;;   )
  ;; )

(defvar org-html-epub-options
  '(
     (:html-footnotes-section        nil nil                 org-html-epub-footnotes-section)
     (:html-format-headline-function nil nil                 org-html-epub-format-headline-function)
     (:html-home/up-format           nil nil                 org-html-epub-home/up-format)
     (:html-inline-image-rules       nil nil                 org-html-epub-inline-image-rules)
     (:html-link-up                  "HTML_LINK_UP" nil      org-html-epub-link-up)
     (:html-link-use-abs-url nil     "html-link-use-abs-url" org-html-epub-link-use-abs-url)
     (:html-postamble-format         nil nil                 org-html-epub-postamble-format)
     (:html-xml-declaration          nil nil                 org-html-epub-xml-declaration)
     ))

(defvar org-html-epub-translate-alist '(
    (headline           . org-html-epub-headline)
    (section            . org-html-epub-section)
    (paragraph          . org-html-epub-paragraph)
    (link               . org-html-epub-link)
    )
  "override transcoders"
  )

(defvar org-html-epub-filters
  '((:filter-parse-tree   . org-html-epub-image-link-filter)
    (:filter-final-output . org-html-epub-final-function)
    )
  " transforms run before / after transcoding.
    args: (str, backend, info)

'org-export-before-processing-hook
'org-export-before-parsing-hook
:filter-parse-tree -> modify on entry of org syntax tree
:filter-body       -> modify output text before template translator
:filter-final-output -> modify final output string
:filter-plain-text, :filter-TYPE

"
  )

(org-export-define-derived-backend 'html-epub 'html
  :translate-alist org-html-epub-translate-alist
  ;; :menu-entry org-html-epub-menu
  :filters-alist org-html-epub-filters
  :options-alist org-html-epub-options
  )

;;-- end backend definition

;;-- autoloads

;;;###autoload
(defun org-html-epub-htmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-html-epub-htmlize-output-type' to `css', calls
to the function `org-html-epub-htmlize-region-for-paste' will
produce code that uses these same face definitions."
  (interactive)
  (unless (require 'htmlize nil t)
    (error "htmlize library missing.  Aborting"))
  (and (get-buffer "*html*") (kill-buffer "*html*"))
  (with-temp-buffer
    (let ((fl (face-list))
	  (htmlize-css-name-prefix "org-")
	  (htmlize-output-type 'css)
	  f i)
      (while (setq f (pop fl)
		   i (and f (face-attribute f :inherit)))
	(when (and (symbolp f) (or (not i) (not (listp i))))
	  (insert (org-add-props (copy-sequence "1") nil 'face f))))
      (htmlize-region (point-min) (point-max))))
  (pop-to-buffer-same-window "*html*")
  (goto-char (point-min))
  (when (re-search-forward "<style" nil t)
    (delete-region (point-min) (match-beginning 0)))
  (when (re-search-forward "</style>" nil t)
    (delete-region (1+ (match-end 0)) (point-max)))
  (beginning-of-line 1)
  (when (looking-at " +") (replace-match ""))
  (goto-char (point-min)))

(provide 'ox-html-epub)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-html.el ends here
