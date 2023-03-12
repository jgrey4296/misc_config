(require 'ox-html)
(require '+ox-html-jg-vars)
(require '+ox-html-jg-utils)
(require '+ox-html-jg-templates)

;;-- backend definition
(org-export-define-backend 'html-jg
  '((bold               . +jg-org-html-bold)
    (center-block       . +jg-org-html-center-block)
    (clock              . +jg-org-html-clock)
    (code               . +jg-org-html-code)
    (drawer             . +jg-org-html-drawer)
    (dynamic-block      . +jg-org-html-dynamic-block)
    (entity             . +jg-org-html-entity)
    (example-block      . +jg-org-html-example-block)
    (export-block       . +jg-org-html-export-block)
    (export-snippet     . +jg-org-html-export-snippet)
    (fixed-width        . +jg-org-html-fixed-width)
    (footnote-reference . +jg-org-html-footnote-reference)
    (headline           . +jg-org-html-headline)
    (horizontal-rule    . +jg-org-html-horizontal-rule)
    (inline-src-block   . +jg-org-html-inline-src-block)
    (inlinetask         . +jg-org-html-inlinetask)
    (inner-template     . +jg-org-html-inner-template)
    (italic             . +jg-org-html-italic)
    (item               . +jg-org-html-item)
    (keyword            . +jg-org-html-keyword)
    (latex-environment  . +jg-org-html-latex-environment)
    (latex-fragment     . +jg-org-html-latex-fragment)
    (line-break         . +jg-org-html-line-break)
    (link               . +jg-org-html-link)
    (node-property      . +jg-org-html-node-property)
    (paragraph          . +jg-org-html-paragraph)
    (plain-list         . +jg-org-html-plain-list)
    (plain-text         . +jg-org-html-plain-text)
    (planning           . +jg-org-html-planning)
    (property-drawer    . +jg-org-html-property-drawer)
    (quote-block        . +jg-org-html-quote-block)
    (radio-target       . +jg-org-html-radio-target)
    (section            . +jg-org-html-section)
    (special-block      . +jg-org-html-special-block)
    (src-block          . +jg-org-html-src-block)
    (statistics-cookie  . +jg-org-html-statistics-cookie)
    (strike-through     . +jg-org-html-strike-through)
    (subscript          . +jg-org-html-subscript)
    (superscript        . +jg-org-html-superscript)
    (table              . +jg-org-html-table)
    (table-cell         . +jg-org-html-table-cell)
    (table-row          . +jg-org-html-table-row)
    (target             . +jg-org-html-target)
    (template           . +jg-org-html-template)
    (timestamp          . +jg-org-html-timestamp)
    (underline          . +jg-org-html-underline)
    (verbatim           . +jg-org-html-verbatim)
    (verse-block        . +jg-org-html-verse-block))
  :filters-alist '((:filter-parse-tree   . +jg-org-html-image-link-filter)
		   (:filter-final-output . +jg-org-html-final-function))
  :menu-entry
  '(?h "Export to HTML"
       ((?J "As jg HTML buffer" +jg-org-html-export-as-html)
	(?j "As jg HTML file"   +jg-org-html-export-to-html)
	(?O "As HTML file and open" (lambda (a s v b)
	                              (if a (+jg-org-html-export-to-html t s v b)
		                        (org-open-file (+jg-org-html-export-to-html nil s v b)))))))
  :options-alist
  '((:html-doctype                   "HTML_DOCTYPE" nil +jg-org-html-doctype)
    (:html-container                 "HTML_CONTAINER" nil +jg-org-html-container-element)
    (:html-content-class             "HTML_CONTENT_CLASS" nil +jg-org-html-content-class)
    (:description                    "DESCRIPTION" nil nil newline)
    (:keywords                       "KEYWORDS" nil nil space)
    (:html-html5-fancy nil           "html5-fancy" +jg-org-html-html5-fancy)
    (:html-link-use-abs-url nil      "html-link-use-abs-url" +jg-org-html-link-use-abs-url)
    (:html-link-home                 "HTML_LINK_HOME" nil +jg-org-html-link-home)
    (:html-link-up                   "HTML_LINK_UP" nil +jg-org-html-link-up)
    (:html-equation-reference-format "HTML_EQUATION_REFERENCE_FORMAT" nil org-html-equation-reference-format t)
    (:html-postamble nil             "html-postamble" +jg-org-html-postamble)
    (:html-preamble nil              "html-preamble" +jg-org-html-preamble)
    (:html-head                      "HTML_HEAD" nil org-html-head newline)
    (:html-head-extra                "HTML_HEAD_EXTRA" nil org-html-head-extra newline)
    (:subtitle                       "SUBTITLE" nil nil parse)
    (:html-head-include-default-style             nil "html-style" nil)
    (:html-head-include-scripts                   nil "html-scripts" nil)
    (:html-allow-name-attribute-in-anchors        nil nil +jg-org-html-allow-name-attribute-in-anchors)
    (:html-divs                                   nil nil +jg-org-html-divs)
    (:html-checkbox-type                          nil nil +jg-org-html-checkbox-type)
    (:html-extension                              nil nil +jg-org-html-extension)
    (:html-footnote-format                        nil nil +jg-org-html-footnote-format)
    (:html-footnote-separator                     nil nil +jg-org-html-footnote-separator)
    (:html-footnotes-section                      nil nil +jg-org-html-footnotes-section)
    (:html-format-drawer-function                 nil nil +jg-org-html-format-drawer-function)
    (:html-format-headline-function               nil nil +jg-org-html-format-headline-function)
    (:html-format-inlinetask-function             nil nil +jg-org-html-format-inlinetask-function)
    (:html-home/up-format                         nil nil +jg-org-html-home/up-format)
    (:html-indent                                 nil nil +jg-org-html-indent)
    (:html-inline-image-rules                     nil nil +jg-org-html-inline-image-rules)
    (:html-link-org-files-as-html                 nil nil +jg-org-html-link-org-files-as-html)
    (:html-metadata-timestamp-format              nil nil +jg-org-html-metadata-timestamp-format)
    (:html-postamble-format                       nil nil +jg-org-html-postamble-format)
    (:html-preamble-format                        nil nil +jg-org-html-preamble-format)
    (:html-prefer-user-labels                     nil nil +jg-org-html-prefer-user-labels)
    (:html-self-link-headlines                    nil nil +jg-org-html-self-link-headlines)
    (:html-table-align-individual-fields          nil nil +jg-org-html-table-align-individual-fields)
    (:html-table-caption-above                    nil nil +jg-org-html-table-caption-above)
    (:html-table-data-tags                        nil nil +jg-org-html-table-data-tags)
    (:html-table-header-tags                      nil nil +jg-org-html-table-header-tags)
    (:html-table-use-header-tags-for-first-column nil nil +jg-org-html-table-use-header-tags-for-first-column)
    (:html-tag-class-prefix                       nil nil +jg-org-html-tag-class-prefix)
    (:html-text-markup-alist                      nil nil +jg-org-html-text-markup-alist)
    (:html-todo-kwd-class-prefix                  nil nil +jg-org-html-todo-kwd-class-prefix)
    (:html-toplevel-hlevel                        nil nil +jg-org-html-toplevel-hlevel)
    (:html-validation-link                        nil nil +jg-org-html-validation-link)
    (:html-viewport                               nil nil +jg-org-html-viewport)
    (:html-inline-images                          nil nil +jg-org-html-inline-images)
    (:html-table-attributes                       nil nil +jg-org-html-table-default-attributes)
    (:html-table-row-open-tag                     nil nil +jg-org-html-table-row-open-tag)
    (:html-table-row-close-tag                    nil nil +jg-org-html-table-row-close-tag)
    (:html-xml-declaration                        nil nil +jg-org-html-xml-declaration)
    (:html-wrap-src-lines                         nil nil +jg-org-html-wrap-src-lines)
    (:html-klipsify-src                           nil nil org-html-klipsify-src)
    (:html-klipse-css                             nil nil org-html-klipse-css)
    (:html-klipse-js                              nil nil org-html-klipse-js)
    (:html-klipse-selection-script                nil nil org-html-klipse-selection-script)
    ;; Redefine regular options.
    (:creator "CREATOR" nil +jg-org-html-creator-string)
    (:with-latex nil "tex" +jg-org-html-with-latex)
    ;; Retrieve LaTeX header for fragments.
    (:latex-header "LATEX_HEADER" nil nil newline)))
;;-- end backend definition

;;-- autoloads
;;;###autoload
(defun +jg-org-html-htmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `+jg-org-html-htmlize-output-type' to `css', calls
to the function `+jg-org-html-htmlize-region-for-paste' will
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
(defun +jg-org-html-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
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
(defun +jg-org-html-convert-region-to-html ()
  "Assume the current region has Org syntax, and convert it to HTML.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in an HTML buffer and use this command
to convert it."
  (interactive)
  (org-export-replace-region-by 'html-jg))

;;;###autoload
(defun +jg-org-html-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
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
		     (when (> (length +jg-org-html-extension) 0) ".")
		     (or (plist-get ext-plist :html-extension)
			 +jg-org-html-extension
			 "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system +jg-org-html-coding-system))
    (org-export-to-file 'html-jg file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun +jg-org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'html-jg filename
		      (concat (when (> (length +jg-org-html-extension) 0) ".")
			      (or (plist-get plist :html-extension)
				  +jg-org-html-extension
				  "html"))
		      plist pub-dir))


;;-- end autoloads

(provide 'ox-html-jg)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-html.el ends here
