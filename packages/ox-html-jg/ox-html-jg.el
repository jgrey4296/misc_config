(require 'org)
(require 'ox-html)
(require 'ox-html-jg-vars)
(require 'ox-html-jg-utils)
(require 'ox-html-jg-templates)

;;-- backend definition
(defvar org-html-jg-transcoders
  '((bold               . org-html-jg-bold)
    (center-block       . org-html-jg-center-block)
    (clock              . org-html-jg-clock)
    (code               . org-html-jg-code)
    (drawer             . org-html-jg-drawer)
    (dynamic-block      . org-html-jg-dynamic-block)
    (entity             . org-html-jg-entity)
    (example-block      . org-html-jg-example-block)
    (export-block       . org-html-jg-export-block)
    (export-snippet     . org-html-jg-export-snippet)
    (fixed-width        . org-html-jg-fixed-width)
    (footnote-reference . org-html-jg-footnote-reference)
    (headline           . org-html-jg-headline)
    (horizontal-rule    . org-html-jg-horizontal-rule)
    (inline-src-block   . org-html-jg-inline-src-block)
    (inlinetask         . org-html-jg-inlinetask)
    (inner-template     . org-html-jg-inner-template)
    (italic             . org-html-jg-italic)
    (item               . org-html-jg-item)
    (keyword            . org-html-jg-keyword)
    (latex-environment  . org-html-jg-latex-environment)
    (latex-fragment     . org-html-jg-latex-fragment)
    (line-break         . org-html-jg-line-break)
    (link               . org-html-jg-link)
    (node-property      . org-html-jg-node-property)
    (paragraph          . org-html-jg-paragraph)
    (plain-list         . org-html-jg-plain-list)
    (plain-text         . org-html-jg-plain-text)
    (planning           . org-html-jg-planning)
    (property-drawer    . org-html-jg-property-drawer)
    (quote-block        . org-html-jg-quote-block)
    (radio-target       . org-html-jg-radio-target)
    (section            . org-html-jg-section)
    (special-block      . org-html-jg-special-block)
    (src-block          . org-html-jg-src-block)
    (statistics-cookie  . org-html-jg-statistics-cookie)
    (strike-through     . org-html-jg-strike-through)
    (subscript          . org-html-jg-subscript)
    (superscript        . org-html-jg-superscript)
    (table              . org-html-jg-table)
    (table-cell         . org-html-jg-table-cell)
    (table-row          . org-html-jg-table-row)
    (target             . org-html-jg-target)
    (template           . org-html-jg-template)
    (timestamp          . org-html-jg-timestamp)
    (underline          . org-html-jg-underline)
    (verbatim           . org-html-jg-verbatim)
    (verse-block        . org-html-jg-verse-block))
)

(defvar org-html-jg-filters
  '((:filter-parse-tree   . org-html-jg-image-link-filter)
    (:filter-final-output . org-html-jg-final-function)
    )
  )

(defvar org-html-jg-menu
  '(?h "Export to HTML"
    ((?J "As jg HTML buffer" org-html-jg-export-as-html)
     (?j "As jg HTML file"   org-html-jg-export-to-html)
     (?O "As HTML file and open" (lambda (a s v b)
	                           (if a (org-html-jg-export-to-html t s v b)
		                     (org-open-file (org-html-jg-export-to-html nil s v b))))))
    )
  )

(defvar org-html-jg-options
  '((:html-doctype                   "HTML_DOCTYPE" nil org-html-jg-doctype)
    (:html-container                 "HTML_CONTAINER" nil org-html-jg-container-element)
    (:html-content-class             "HTML_CONTENT_CLASS" nil org-html-jg-content-class)
    (:description                    "DESCRIPTION" nil nil newline)
    (:keywords                       "KEYWORDS" nil nil space)
    (:html-html5-fancy nil           "html5-fancy" org-html-jg-html5-fancy)
    (:html-link-use-abs-url nil      "html-link-use-abs-url" org-html-jg-link-use-abs-url)
    (:html-link-home                 "HTML_LINK_HOME" nil org-html-jg-link-home)
    (:html-link-up                   "HTML_LINK_UP" nil org-html-jg-link-up)
    (:html-equation-reference-format "HTML_EQUATION_REFERENCE_FORMAT" nil org-html-equation-reference-format t)
    (:html-postamble nil             "html-postamble" org-html-jg-postamble)
    (:html-preamble nil              "html-preamble" org-html-jg-preamble)
    (:html-head                      "HTML_HEAD" nil org-html-head newline)
    (:html-head-extra                "HTML_HEAD_EXTRA" nil org-html-head-extra newline)
    (:subtitle                       "SUBTITLE" nil nil parse)
    (:html-head-include-default-style             nil "html-style" nil)
    (:html-head-include-scripts                   nil "html-scripts" nil)
    (:html-allow-name-attribute-in-anchors        nil nil org-html-jg-allow-name-attribute-in-anchors)
    (:html-divs                                   nil nil org-html-jg-divs)
    (:html-checkbox-type                          nil nil org-html-jg-checkbox-type)
    (:html-extension                              nil nil org-html-jg-extension)
    (:html-footnote-format                        nil nil org-html-jg-footnote-format)
    (:html-footnote-separator                     nil nil org-html-jg-footnote-separator)
    (:html-footnotes-section                      nil nil org-html-jg-footnotes-section)
    (:html-format-drawer-function                 nil nil org-html-jg-format-drawer-function)
    (:html-format-headline-function               nil nil org-html-jg-format-headline-function)
    (:html-format-inlinetask-function             nil nil org-html-jg-format-inlinetask-function)
    (:html-home/up-format                         nil nil org-html-jg-home/up-format)
    (:html-indent                                 nil nil org-html-jg-indent)
    (:html-inline-image-rules                     nil nil org-html-jg-inline-image-rules)
    (:html-link-org-files-as-html                 nil nil org-html-jg-link-org-files-as-html)
    (:html-metadata-timestamp-format              nil nil org-html-jg-metadata-timestamp-format)
    (:html-postamble-format                       nil nil org-html-jg-postamble-format)
    (:html-preamble-format                        nil nil org-html-jg-preamble-format)
    (:html-prefer-user-labels                     nil nil org-html-jg-prefer-user-labels)
    (:html-self-link-headlines                    nil nil org-html-jg-self-link-headlines)
    (:html-table-align-individual-fields          nil nil org-html-jg-table-align-individual-fields)
    (:html-table-caption-above                    nil nil org-html-jg-table-caption-above)
    (:html-table-data-tags                        nil nil org-html-jg-table-data-tags)
    (:html-table-header-tags                      nil nil org-html-jg-table-header-tags)
    (:html-table-use-header-tags-for-first-column nil nil org-html-jg-table-use-header-tags-for-first-column)
    (:html-tag-class-prefix                       nil nil org-html-jg-tag-class-prefix)
    (:html-text-markup-alist                      nil nil org-html-jg-text-markup-alist)
    (:html-todo-kwd-class-prefix                  nil nil org-html-jg-todo-kwd-class-prefix)
    (:html-toplevel-hlevel                        nil nil org-html-jg-toplevel-hlevel)
    (:html-validation-link                        nil nil org-html-jg-validation-link)
    (:html-viewport                               nil nil org-html-jg-viewport)
    (:html-inline-images                          nil nil org-html-jg-inline-images)
    (:html-table-attributes                       nil nil org-html-jg-table-default-attributes)
    (:html-table-row-open-tag                     nil nil org-html-jg-table-row-open-tag)
    (:html-table-row-close-tag                    nil nil org-html-jg-table-row-close-tag)
    (:html-xml-declaration                        nil nil org-html-jg-xml-declaration)
    (:html-wrap-src-lines                         nil nil org-html-jg-wrap-src-lines)
    (:html-klipsify-src                           nil nil org-html-klipsify-src)
    (:html-klipse-css                             nil nil org-html-klipse-css)
    (:html-klipse-js                              nil nil org-html-klipse-js)
    (:html-klipse-selection-script                nil nil org-html-klipse-selection-script)
    ;; Redefine regular options.
    (:creator "CREATOR" nil org-html-jg-creator-string)
    (:with-latex nil "tex" org-html-jg-with-latex)
    ;; Retrieve LaTeX header for fragments.
    (:latex-header "LATEX_HEADER" nil nil newline))
  )

(org-export-define-backend 'html-jg
  org-html-jg-transcoders
  :filters-alist org-html-jg-filters
  :menu-entry org-html-jg-menu
  :options-alist org-html-jg-options
  )
;;-- end backend definition

;;-- autoloads
;;;###autoload
(defun org-html-jg-htmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-html-jg-htmlize-output-type' to `css', calls
to the function `org-html-jg-htmlize-region-for-paste' will
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

;;;###autoload
(defun org-html-jg-export-as-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'html-jg "*Org HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-html-jg-convert-region-to-html ()
  "Assume the current region has Org syntax, and convert it to HTML.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in an HTML buffer and use this command
to convert it."
  (interactive)
  (org-export-replace-region-by 'html-jg))

;;;###autoload
(defun org-html-jg-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat
		     (when (> (length org-html-jg-extension) 0) ".")
		     (or (plist-get ext-plist :html-extension)
			 org-html-jg-extension
			 "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-jg-coding-system))
    (org-export-to-file 'html-jg file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-html-jg-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'html-jg filename
		      (concat (when (> (length org-html-jg-extension) 0) ".")
			      (or (plist-get plist :html-extension)
				  org-html-jg-extension
				  "html"))
		      plist pub-dir))

;;-- end autoloads

(provide 'ox-html-jg)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-html.el ends here
