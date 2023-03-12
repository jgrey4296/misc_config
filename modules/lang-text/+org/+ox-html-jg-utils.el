;;; +ox-html-jg-utils.el -*- lexical-binding: t; -*-

;;-- internal

(defun +jg-org-html--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets."
  (let* ((type (org-element-type datum))
	 (user-label
	  (org-element-property
	   (pcase type
	     ((or `headline `inlinetask) :CUSTOM_ID)
	     ((or `radio-target `target) :value)
	     (_ :name))
	   datum)))
    (cond
     ((and user-label
	   (or (plist-get info :html-prefer-user-labels)
	       ;; Used CUSTOM_ID property unconditionally.
	       (memq type '(headline inlinetask))))
      user-label)
     ((and named-only
	   (not (memq type '(headline inlinetask radio-target target)))
	   (not user-label))
      nil)
     (t
      (org-export-get-reference datum info)))))

(defun +jg-org-html--wrap-image (contents info &optional caption label)
  "Wrap CONTENTS string within an appropriate environment for images.
INFO is a plist used as a communication channel.  When optional
arguments CAPTION and LABEL are given, use them for caption and
\"id\" attribute."
  (let ((html5-fancy (org-html--html5-fancy-p info)))
    (format (if html5-fancy "\n<figure%s>\n%s%s\n</figure>"
	      "\n<div%s class=\"figure\">\n%s%s\n</div>")
	    ;; ID.
	    (if (org-string-nw-p label) (format " id=\"%s\"" label) "")
	    ;; Contents.
	    (if html5-fancy contents (format "<p>%s</p>" contents))
	    ;; Caption.
	    (if (not (org-string-nw-p caption)) ""
	      (format (if html5-fancy "\n<figcaption>%s</figcaption>"
			"\n<p>%s</p>")
		      caption)))))

(defun +jg-org-html--format-image (source attributes info)
  "Return \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by
`org-export-read-attribute'.  INFO is a plist used as
a communication channel."
  (org-html-close-tag
   "img"
   (org-html--make-attribute-string
    (org-combine-plists
     (list :src source
           :alt (if (string-match-p
                     (concat "^" org-preview-latex-image-directory) source)
                    (+jg-org-html-encode-plain-text
                     (org-find-text-property-in-string 'org-latex-src source))
                  (file-name-nondirectory source)))
     (if (string= "svg" (file-name-extension source))
         (org-combine-plists '(:class "org-svg") attributes '(:fallback nil))
       attributes)))
   info))

(defun +jg-org-html--textarea-block (element)
  "Transcode ELEMENT into a textarea block.
ELEMENT is either a source or an example block."
  (let* ((code (car (org-export-unravel-code element)))
	 (attr (org-export-read-attribute :attr_html element)))
    (format "<p>\n<textarea cols=\"%s\" rows=\"%s\">\n%s</textarea>\n</p>"
	    (or (plist-get attr :width) 80)
	    (or (plist-get attr :height) (org-count-lines code))
	    code)))

;;-- end internal

;;-- table
(defun +jg-org-html-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (pcase (org-export-collect-footnote-definitions info)
    (`nil nil)
    (definitions
      (format
       (plist-get info :html-footnotes-section)
       (org-html--translate "Footnotes" info)
       (format
	"\n%s\n"
	(mapconcat
	 (lambda (definition)
	   (pcase definition
	     (`(,n ,_ ,def)
	      ;; `org-export-collect-footnote-definitions' can return
	      ;; two kinds of footnote definitions: inline and blocks.
	      ;; Since this should not make any difference in the HTML
	      ;; output, we wrap the inline definitions within
	      ;; a "footpara" class paragraph.
	      (let ((inline? (not (org-element-map def org-element-all-elements
				    #'identity nil t)))
		    (anchor (+jg-org-html--anchor
			     (format "fn.%d" n)
			     n
			     (format " class=\"footnum\" href=\"#fnr.%d\" role=\"doc-backlink\"" n)
			     info))
		    (contents (org-trim (org-export-data def info))))
		(format "<div class=\"footdef\">%s %s</div>\n"
			(format (plist-get info :html-footnote-format) anchor)
			(format "<div class=\"footpara\" role=\"doc-footnote\">%s</div>"
				(if (not inline?) contents
				  (format "<p class=\"footpara\">%s</p>"
					  contents))))))))
	 definitions
	 "\n"))))))

;;-- end table

;;-- template building

(defun +jg-org-html--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let* ((title (+jg-org-html-plain-text
		 (org-element-interpret-data (plist-get info :title)) info))
	 ;; Set title to an invisible character instead of leaving it
	 ;; empty, which is invalid.
	 (title (if (org-string-nw-p title) title "&lrm;"))
	 (charset (or (and +jg-org-html-coding-system
			   (symbol-name
			    (coding-system-get +jg-org-html-coding-system
					       'mime-charset)))
		      "iso-8859-1")))
    (concat
     (when (plist-get info :time-stamp-file)
       (format-time-string
	(concat "<!-- "
		(plist-get info :html-metadata-timestamp-format)
		" -->\n")))

     (if (org-html-html5-p info)
	 (org-html--build-meta-entry "charset" charset)
       (org-html--build-meta-entry "http-equiv" "Content-Type"
				   (concat "text/html;charset=" charset)))

     (let ((viewport-options
	    (cl-remove-if-not (lambda (cell) (org-string-nw-p (cadr cell)))
			      (plist-get info :html-viewport))))
       (if viewport-options
	   (org-html--build-meta-entry "name" "viewport"
				       (mapconcat
					(lambda (elm)
                                          (format "%s=%s" (car elm) (cadr elm)))
					viewport-options ", "))))

     (format "<title>%s</title>\n" title)

     (mapconcat
      (lambda (args) (apply #'org-html--build-meta-entry args))
      (delq nil (if (functionp +jg-org-html-meta-tags)
		    (funcall +jg-org-html-meta-tags info)
		  +jg-org-html-meta-tags))
      ""))))

(defun +jg-org-html--build-head (info)
  "Return information for the <head>..</head> of the HTML output.
INFO is a plist used as a communication channel."
  (org-element-normalize-string
   (concat
    (org-element-normalize-string (plist-get info :html-head))
    (org-element-normalize-string (plist-get info :html-head-extra))
    (when (and (plist-get info :html-htmlized-css-url)
	       (eq +jg-org-html-htmlize-output-type 'css))
      (org-html-close-tag "link"
			  (format "rel=\"stylesheet\" href=\"%s\" type=\"text/css\""
				  (plist-get info :html-htmlized-css-url))
			  info))
    )))

(defun +jg-org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  (let ((timestamp-format (plist-get info :html-metadata-timestamp-format)))
    `((?t . ,(org-export-data (plist-get info :title) info))
      (?s . ,(org-export-data (plist-get info :subtitle) info))
      (?d . ,(org-export-data (org-export-get-date info timestamp-format)
			      info))
      (?T . ,(format-time-string timestamp-format))
      (?a . ,(org-export-data (plist-get info :author) info))
      (?e . ,(mapconcat
	      (lambda (e) (format "<a href=\"mailto:%s\">%s</a>" e e))
	      (split-string (plist-get info :email)  ",+ *")
	      ", "))
      (?c . ,(plist-get info :creator))
      (?C . ,(let ((file (plist-get info :input-file)))
	       (format-time-string timestamp-format
				   (and file (file-attribute-modification-time
					      (file-attributes file))))))
      (?v . ,(or (plist-get info :html-validation-link) "")))))

(defun +jg-org-html--build-pre/postamble (type info)
  "Return document preamble or postamble as a string, or nil.
TYPE is either `preamble' or `postamble', INFO is a plist used as a
communication channel."
  (let ((section (plist-get info (intern (format ":html-%s" type))))
	(spec (+jg-org-html-format-spec info)))
    (when section
      (let ((section-contents
	     (if (functionp section) (funcall section info)
	       (cond
		((stringp section) (format-spec section spec))
		((eq section 'auto)
		 (let ((date (cdr (assq ?d spec)))
		       (author (cdr (assq ?a spec)))
		       (email (cdr (assq ?e spec)))
		       (creator (cdr (assq ?c spec)))
		       (validation-link (cdr (assq ?v spec))))
		   (concat
		    (and (plist-get info :with-date)
			 (org-string-nw-p date)
			 (format "<p class=\"date\">%s: %s</p>\n"
				 (org-html--translate "Date" info)
				 date))
		    (and (plist-get info :with-author)
			 (org-string-nw-p author)
			 (format "<p class=\"author\">%s: %s</p>\n"
				 (org-html--translate "Author" info)
				 author))
		    (and (plist-get info :with-email)
			 (org-string-nw-p email)
			 (format "<p class=\"email\">%s: %s</p>\n"
				 (org-html--translate "Email" info)
				 email))
		    (and (plist-get info :time-stamp-file)
			 (format
			  "<p class=\"date\">%s: %s</p>\n"
			  (org-html--translate "Created" info)
			  (format-time-string
			   (plist-get info :html-metadata-timestamp-format))))
		    (and (plist-get info :with-creator)
			 (org-string-nw-p creator)
			 (format "<p class=\"creator\">%s</p>\n" creator))
		    (and (org-string-nw-p validation-link)
			 (format "<p class=\"validation\">%s</p>\n"
				 validation-link)))))
		(t
		 (let ((formats (plist-get info (if (eq type 'preamble)
						    :html-preamble-format
						  :html-postamble-format)))
		       (language (plist-get info :language)))
		   (format-spec
		    (cadr (or (assoc-string language formats t)
			      (assoc-string "en" formats t)))
		    spec)))))))
	(let ((div (assq type (plist-get info :html-divs))))
	  (when (org-string-nw-p section-contents)
	    (concat
	     (format "<%s id=\"%s\" class=\"%s\">\n"
		     (nth 1 div)
		     (nth 2 div)
		     +jg-org-html--pre/postamble-class)
	     (org-element-normalize-string section-contents)
	     (format "</%s>\n" (nth 1 div)))))))))

(defun +jg-org-html-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (+jg-org-html-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (+jg-org-html-footnote-section info)))

(defun +jg-org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	    (decl (or (and (stringp xml-declaration) xml-declaration)
		      (cdr (assoc (plist-get info :html-extension)
				  xml-declaration))
		      (cdr (assoc "html" xml-declaration))
		      "")))
       (when (not (or (not decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and +jg-org-html-coding-system
                                  ;; FIXME: Use Emacs 22 style here, see `coding-system-get'.
				  (coding-system-get +jg-org-html-coding-system 'mime-charset))
			     "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (cond ((org-html-xhtml-p info)
		  (format
		   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
		   (plist-get info :language) (plist-get info :language)))
		 ((org-html-html5-p info)
		  (format " lang=\"%s\"" (plist-get info :language))))
	   ">\n")
   "<head>\n"
   (+jg-org-html--build-meta-info info)
   (+jg-org-html--build-head info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Preamble.
   (+jg-org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\" class=\"%s\">\n"
             (nth 1 div)
             (nth 2 div)
             (plist-get info :html-content-class)))
   ;; Document title.
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
		       (plist-get info :title)))
	   (subtitle (plist-get info :subtitle))
	   (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	 (format
	  (if html5-fancy
	      "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
	    "<h1 class=\"title\">%s%s</h1>\n")
	  (org-export-data title info)
	  (if subtitle
	      (format
	       (if html5-fancy
		   "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
		 (concat "\n" (org-html-close-tag "br" nil info) "\n"
			 "<span class=\"subtitle\">%s</span>\n"))
	       (org-export-data subtitle info))
	    "")))))
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (+jg-org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
	     "</script><script src=\""
	     +org-html-klipse-js
	     "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
	     +org-html-klipse-css "\"/>"))
   ;; Closing document.
   "</body>\n</html>"))

;;-- end template building

;;-- misc
(defun +jg-org-html--anchor (id desc attributes info)
  "Format a HTML anchor."
  (let* ((name (and (plist-get info :html-allow-name-attribute-in-anchors) id))
	 (attributes (concat (and id (format " id=\"%s\"" id))
			     (and name (format " name=\"%s\"" name))
			     attributes)))
    (format "<a%s>%s</a>" attributes (or desc ""))))

(defun +jg-org-html--todo (todo info)
  "Format TODO keywords into HTML."
  (when todo
    (format "<span class=\"%s %s%s\">%s</span>"
	    (if (member todo org-done-keywords) "done" "todo")
	    (or (plist-get info :html-todo-kwd-class-prefix) "")
	    (org-html-fix-class-name todo)
	    todo)))

(defun +jg-org-html--priority (priority _info)
  "Format a priority into HTML.
PRIORITY is the character code of the priority or nil.  INFO is
a plist containing export options."
  (and priority (format "<span class=\"priority\">[%c]</span>" priority)))

(defun +jg-org-html--tags (tags info)
  "Format TAGS into HTML.
INFO is a plist containing export options."
  (when tags
    (format "<span class=\"tag\">%s</span>"
	    (mapconcat
	     (lambda (tag)
	       (format "<span class=\"%s\">%s</span>"
		       (concat (plist-get info :html-tag-class-prefix)
			       (org-html-fix-class-name tag))
		       tag))
	     tags "&#xa0;"))))

;;-- end misc

(provide '+ox-html-jg-utils)
