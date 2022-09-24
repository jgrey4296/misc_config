;;; +ox-html-jg-vars.el -*- lexical-binding: t; -*-

;;-- prelude
;;; ox-html.el --- HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2022 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;;      Jambunathan K <kjambunathan at gmail dot com>
;; Maintainer: TEC <tecosaur@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a HTML back-end for Org generic exporter.
;; See Org manual for more information.

;;; Code:

;;; Dependencies

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'format-spec)
(require 'ox)
(require 'ox-publish)
(require 'table)

;;-- end prelude

;;-- pre-declarations
(declare-function org-id-find-id-file "org-id" (id))
(declare-function htmlize-region "ext:htmlize" (beg end))
(declare-function mm-url-decode-entities "mm-url" ())
(declare-function org-at-heading-p "org" (&optional _))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-next-visible-heading "org" (arg))

(defvar htmlize-css-name-prefix)
(defvar htmlize-output-type)
(defvar htmlize-output-type)
(defvar htmlize-css-name-prefix)
;;-- end pre-declarations


;;-- internal variables
(defvar +jg-org-html-format-table-no-css)

(defvar +jg-org-html--pre/postamble-class "status"
  "CSS class used for pre/postamble.")

;;-- end internal variables

(defgroup jg-org-export-html nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export HTML"
  :group 'org-export)

;;-- text-formatting
(defcustom +jg-org-html-text-markup-alist
  '((bold           . "<b>%s</b>")
    (code           . "<code>%s</code>")
    (italic         . "<i>%s</i>")
    (strike-through . "<del>%s</del>")
    (underline      . "<span class=\"underline\">%s</span>")
    (verbatim       . "<code>%s</code>"))
  "Alist of HTML expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underline' and `verbatim'.  The value is
a formatting string to wrap fontified text with.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(alist :key-type (symbol :tag "Markup type")
		:value-type (string :tag "Format string"))
  :options '(bold code italic strike-through underline verbatim))

(defvar +jg-org-html-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `+jg-org-html-encode-plain-text'.")

(defcustom +jg-org-html-htmlize-font-prefix "org-"
  "The prefix for CSS class names for htmlize font specifications."
  :group 'jg-org-export-html
  :type 'string)

(defcustom +jg-org-html-htmlize-output-type 'css
  "Output type to be used by htmlize when formatting code snippets.
Choices are `css' to export the CSS selectors only,`inline-css'
to export the CSS attribute values inline in the HTML or nil to
export plain text.  We use as default `inline-css', in order to
make the resulting HTML self-containing.

However, this will fail when using Emacs in batch mode for export, because
then no rich font definitions are in place.  It will also not be good if
people with different Emacs setup contribute HTML files to a website,
because the fonts will represent the individual setups.  In these cases,
it is much better to let Org/Htmlize assign classes only, and to use
a style file to define the look of these classes.
To get a start for your css file, start Emacs session and make sure that
all the faces you are interested in are defined, for example by loading files
in all modes you want.  Then, use the command
`\\[+jg-org-html-htmlize-generate-css]' to extract class definitions."
  :group 'jg-org-export-html
  :type '(choice (const css) (const inline-css) (const nil)))

;;-- end text-formatting

;;-- indentation
(defcustom +jg-org-html-indent nil
  "Non-nil means to indent the generated HTML.
Warning: non-nil may break indentation of source code blocks."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)
;;-- end indentation

;;-- drawers
(defcustom +jg-org-html-format-drawer-function (lambda (_name contents) contents)
  "Function called to format a drawer in HTML code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

The default value simply returns the value of CONTENTS."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)
;;-- end drawers

;;-- footnotes
(defcustom +jg-org-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'jg-org-export-html
  :type 'string)

(defcustom +jg-org-html-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'jg-org-export-html
  :type 'string)

(defcustom +jg-org-html-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'jg-org-export-html
  :type 'string)

;;-- end footnotes

;;-- headlines
(defcustom +jg-org-html-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title."
  :group 'jg-org-export-html
  :type 'integer)

(defcustom +jg-org-html-format-headline-function
  '+jg-org-html-format-headline-default-function
  "Function to format headline text.

This function will be called with six arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags (string or nil).
INFO      the export options (plist).

The function result will be used in the section format string."
  :group 'jg-org-export-html
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'function)

;;-- end headlines

;;-- html specific
(defcustom +jg-org-html-allow-name-attribute-in-anchors nil
  "When nil, do not set \"name\" attribute in anchors.
By default, when appropriate, anchors are formatted with \"id\"
but without \"name\" attribute."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom +jg-org-html-self-link-headlines nil
  "When non-nil, the headlines contain a hyperlink to themselves."
  :group 'jg-org-export-html
  :package-version '(Org . "9.3")
  :type 'boolean
  :safe #'booleanp)

(defcustom +jg-org-html-prefer-user-labels nil
  "When non-nil use user-defined names and ID over internal ones.

By default, Org generates its own internal ID values during HTML
export.  This process ensures that these values are unique and
valid, but the keys are not available in advance of the export
process, and not so readable.

When this variable is non-nil, Org will use NAME keyword, or the
real name of the target to create the ID attribute.

Independently of this variable, however, CUSTOM_ID are always
used as a reference."
  :group 'jg-org-export-html
  :package-version '(Org . "9.4")
  :type 'boolean
  :safe #'booleanp)

;;-- end html specific

;;-- inline tasks
(defcustom +jg-org-html-format-inlinetask-function
  '+jg-org-html-format-inlinetask-default-function
  "Function called to format an inlinetask in HTML code.

The function must accept seven parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.
  INFO      the export options, as a plist

The function should return the string to be exported."
  :group 'jg-org-export-html
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'function)

;;-- end inline tasks

;;-- latex

(defcustom +jg-org-html-with-latex org-export-with-latex
  "Non-nil means process LaTeX math snippets.

When set, the exporter will process LaTeX environments and
fragments.

This option can also be set with the +OPTIONS line,
e.g. \"tex:mathjax\".  Allowed values are:

  nil           Ignore math snippets.
  `verbatim'    Keep everything in verbatim
  `mathjax', t  Do MathJax preprocessing and arrange for MathJax.js to
                be loaded.
  `html'        Use `org-latex-to-html-convert-command' to convert
                LaTeX fragments to HTML.
  SYMBOL        Any symbol defined in `org-preview-latex-process-alist',
                e.g., `dvipng'."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Do not process math in any way" nil)
	  (const :tag "Leave math verbatim" verbatim)
	  (symbol :tag "Convert to image to display math" :value dvipng)))
;;-- end latex

;;-- links
(defcustom +jg-org-html-link-org-files-as-html t
  "Non-nil means make file links to \"file.org\" point to \"file.html\".

When Org mode is exporting an Org file to HTML, links to non-HTML files
are directly put into a \"href\" tag in HTML.  However, links to other Org files
(recognized by the extension \".org\") should become links to the corresponding
HTML file, assuming that the linked Org file will also be converted to HTML.

When nil, the links still point to the plain \".org\" file."
  :group 'jg-org-export-html
  :type 'boolean)
;;-- end links

;;-- images
(defcustom +jg-org-html-inline-images t
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.1")
  :type 'boolean)

(defcustom +jg-org-html-inline-image-rules
  `(("file" . ,(regexp-opt '(".jpeg" ".jpg" ".png" ".gif" ".svg" ".webp")))
    ("http" . ,(regexp-opt '(".jpeg" ".jpg" ".png" ".gif" ".svg" ".webp")))
    ("https" . ,(regexp-opt '(".jpeg" ".jpg" ".png" ".gif" ".svg" ".webp"))))
  "Rules characterizing image files that can be inlined into HTML.
A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path."
  :group 'jg-org-export-html
  :package-version '(Org . "9.5")
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))
;;-- end images

;;-- src block
(defcustom +jg-org-html-wrap-src-lines nil
  "If non-nil, wrap individual lines of source blocks in \"code\" elements.
In this case, add line number in attribute \"data-ox-html-linenr\" when line
numbers are enabled."
  :group 'jg-org-export-html
  :package-version '(Org . "9.3")
  :type 'boolean
  :safe #'booleanp)
;;-- end src block

;;-- tables
(defcustom +jg-org-html-table-default-attributes
  '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides")
  "Default attributes and values which will be used in table tags.
This is a plist where attributes are symbols, starting with
colons, and values are strings.

When exporting to HTML5, these values will be disregarded."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(plist :key-type (symbol :tag "Property")
		:value-type (string :tag "Value")))

(defcustom +jg-org-html-table-header-tags '("<th scope=\"%s\"%s>" . "</th>")
  "The opening and ending tags for table header fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `+jg-org-html-table-use-header-tags-for-first-column'.
See also the variable `+jg-org-html-table-align-individual-fields'."
  :group 'jg-org-export-html
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom +jg-org-html-table-data-tags '("<td%s>" . "</td>")
  "The opening and ending tags for table data fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `+jg-org-html-table-align-individual-fields'."
  :group 'jg-org-export-html
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom +jg-org-html-table-row-open-tag "<tr>"
  "The opening tag for table rows.
This is customizable so that alignment options can be specified.
Instead of strings, these can be a Lisp function that will be
evaluated for each row in order to construct the table row tags.

The function will be called with these arguments:

         `number': row number (0 is the first row)
   `group-number': group number of current row
   `start-group?': non-nil means the row starts a group
     `end-group?': non-nil means the row ends a group
           `top?': non-nil means this is the top row
        `bottom?': non-nil means this is the bottom row

For example:

  (setq +jg-org-html-table-row-open-tag
        (lambda (number group-number start-group? end-group-p top? bottom?)
           (cond (top? \"<tr class=\\\"tr-top\\\">\")
                 (bottom? \"<tr class=\\\"tr-bottom\\\">\")
                 (t (if (= (mod number 2) 1)
                        \"<tr class=\\\"tr-odd\\\">\"
                      \"<tr class=\\\"tr-even\\\">\")))))

will use the \"tr-top\" and \"tr-bottom\" classes for the top row
and the bottom row, and otherwise alternate between \"tr-odd\" and
\"tr-even\" for odd and even rows."
  :group 'jg-org-export-html
  :type '(choice :tag "Opening tag"
		 (string :tag "Specify")
		 (function)))

(defcustom +jg-org-html-table-row-close-tag "</tr>"
  "The closing tag for table rows.
This is customizable so that alignment options can be specified.
Instead of strings, this can be a Lisp function that will be
evaluated for each row in order to construct the table row tags.

See documentation of `+jg-org-html-table-row-open-tag'."
  :group 'jg-org-export-html
  :type '(choice :tag "Closing tag"
		 (string :tag "Specify")
		 (function)))

(defcustom +jg-org-html-table-align-individual-fields t
  "Non-nil means attach style attributes for alignment to each table field.
When nil, alignment will only be specified in the column tags, but this
is ignored by some browsers (like Firefox, Safari).  Opera does it right
though."
  :group 'jg-org-export-html
  :type 'boolean)

(defcustom +jg-org-html-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'jg-org-export-html
  :type 'boolean)

(defcustom +jg-org-html-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'jg-org-export-html
  :type 'boolean)
;;-- end tables

;;-- tags
(defcustom +jg-org-html-tag-class-prefix ""
  "Prefix to class names for TODO keywords.
Each tag gets a class given by the tag itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'jg-org-export-html
  :type 'string)
;;-- end tags

;;-- html file settings
(defcustom +jg-org-html-extension "html"
  "The extension for exported HTML files."
  :group 'jg-org-export-html
  :type 'string)

(defcustom +jg-org-html-xml-declaration
  '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>"))
  "The extension for exported HTML files.
%s will be replaced with the charset of the exported file.
This may be a string, or an alist with export extensions
and corresponding declarations.

This declaration only applies when exporting to XHTML."
  :group 'jg-org-export-html
  :type '(choice
	  (string :tag "Single declaration")
	  (repeat :tag "Dependent on extension"
		  (cons (string :tag "Extension")
			(string :tag "Declaration")))))

(defcustom +jg-org-html-coding-system 'utf-8
  "Coding system for HTML export.
Use utf-8 as the default value."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'coding-system)

(defcustom +jg-org-html-doctype "xhtml-strict"
  "Document type definition to use for exported HTML files.
Can be set with the in-buffer HTML_DOCTYPE property or for
publishing, with :html-doctype."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type (append
	 '(choice)
	 (mapcar (lambda (x) `(const ,(car x))) org-html-doctype-alist)
	 '((string :tag "Custom doctype" ))))

(defcustom +jg-org-html-html5-fancy nil
  "Non-nil means using new HTML5 elements.
This variable is ignored for anything other than HTML5 export."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom +jg-org-html-viewport '((width "device-width")
			       (initial-scale "1")
			       (minimum-scale "")
			       (maximum-scale "")
			       (user-scalable ""))
  "Viewport options for mobile-optimized sites.

The following values are recognized

width          Size of the viewport.
initial-scale  Zoom level when the page is first loaded.
minimum-scale  Minimum allowed zoom level.
maximum-scale  Maximum allowed zoom level.
user-scalable  Whether zoom can be changed.

The viewport meta tag is inserted if this variable is non-nil.

See the following site for a reference:
https://developer.mozilla.org/en-US/docs/Mozilla/Mobile/Viewport_meta_tag"
  :group 'jg-org-export-html
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice (const :tag "Disable" nil)
		 (list :tag "Enable"
		       (list :tag "Width of viewport"
			     (const :format "             " width)
			     (choice (const :tag "unset" "")
				     (string)))
		       (list :tag "Initial scale"
			     (const :format "             " initial-scale)
			     (choice (const :tag "unset" "")
				     (string)))
		       (list :tag "Minimum scale/zoom"
			     (const :format "             " minimum-scale)
			     (choice (const :tag "unset" "")
				     (string)))
		       (list :tag "Maximum scale/zoom"
			     (const :format "             " maximum-scale)
			     (choice (const :tag "unset" "")
				     (string)))
		       (list :tag "User scalable/zoomable"
			     (const :format "             " user-scalable)
			     (choice (const :tag "unset" "")
				     (const "true")
				     (const "false"))))))

;;-- end html file settings

;;-- metadata
(defcustom +jg-org-html-meta-tags #'org-html-meta-tags-default
  "Form that is used to produce meta tags in the HTML head.

Can be a list where each item is a list of arguments to be passed
to `+jg-org-html--build-meta-entry'.  Any nil items are ignored.

Also accept a function which gives such a list when called with a
single argument (INFO, a communication plist)."
  :group 'jg-org-export-html
  :package-version '(Org . "9.5")
  :type '(choice
	  (repeat
	   (list (string :tag "Meta label")
		 (string :tag "label value")
		 (string :tag "Content value")))
	  function))

;;-- end metadata

;;-- content settings
(defcustom +jg-org-html-container-element "div"
  "HTML element to use for wrapping top level sections.
Can be set with the in-buffer HTML_CONTAINER property or for
publishing, with :html-container.

Note that changing the default will prevent you from using
org-info.js for your website."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom +jg-org-html-content-class "content"
  "CSS class name to use for the top level content wrapper.
Can be set with the in-buffer HTML_CONTENT_CLASS property or for
publishing, with :html-content-class."
  :group 'jg-org-export-html
  :version "27.2"
  :package-version '(Org . "9.5")
  :type 'string)

(defcustom +jg-org-html-divs
  '((preamble  "div" "preamble")
    (content   "div" "content")
    (postamble "div" "postamble"))
  "Alist of the three section elements for HTML export.
The car of each entry is one of `preamble', `content' or `postamble'.
The cdrs of each entry are the ELEMENT_TYPE and ID for each
section of the exported document.

Note that changing the default will prevent you from using
org-info.js for your website."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(list :greedy t
	       (list :tag "Preamble"
		     (const :format "" preamble)
		     (string :tag "element") (string :tag "     id"))
	       (list :tag "Content"
		     (const :format "" content)
		     (string :tag "element") (string :tag "     id"))
	       (list :tag "Postamble" (const :format "" postamble)
		     (string :tag "     id") (string :tag "element"))))

(defconst +jg-org-html-checkbox-types
  '((unicode .
             ((on . "&#x2611;") (off . "&#x2610;") (trans . "&#x2610;")))
    (ascii .
           ((on . "<code>[X]</code>")
            (off . "<code>[&#xa0;]</code>")
            (trans . "<code>[-]</code>")))
    (html .
	  ((on . "<input type='checkbox' checked='checked' />")
	   (off . "<input type='checkbox' />")
	   (trans . "<input type='checkbox' />"))))
  "Alist of checkbox types.
The cdr of each entry is an alist list three checkbox types for
HTML export: `on', `off' and `trans'.

The choices are:
  `unicode' Unicode characters (HTML entities)
  `ascii'   ASCII characters
  `html'    HTML checkboxes

Note that only the ascii characters implement tri-state
checkboxes.  The other two use the `off' checkbox for `trans'.")

(defcustom +jg-org-html-checkbox-type 'ascii
  "The type of checkboxes to use for HTML export.
See `+jg-org-html-checkbox-types' for the values used for each
option."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "ASCII characters" ascii)
	  (const :tag "Unicode characters" unicode)
	  (const :tag "HTML checkboxes" html)))

(defcustom +jg-org-html-metadata-timestamp-format "%Y-%m-%d %a %H:%M"
  "Format used for timestamps in preamble, postamble and metadata.
See `format-time-string' for more information on its components."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)
;;-- end content settings

;;-- postamble
(defcustom +jg-org-html-postamble 'auto
  "Non-nil means insert a postamble in HTML export.

When set to `auto', check against the
`org-export-with-author/email/creator/date' variables to set the
content of the postamble.  When set to a string, use this string
as the postamble.  When t, insert a string as defined by the
formatting string in `+jg-org-html-postamble-format'.

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting :html-postamble in publishing projects will take
precedence over this variable."
  :group 'jg-org-export-html
  :type '(choice (const :tag "No postamble" nil)
		 (const :tag "Auto postamble" auto)
		 (const :tag "Default formatting string" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom +jg-org-html-postamble-format
  '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">%c</p>
<p class=\"validation\">%v</p>"))
  "Alist of languages and format strings for the HTML postamble.

The first element of each list is the language code, as used for
the LANGUAGE keyword.  See `org-export-default-language'.

The second element of each list is a format string to format the
postamble itself.  This format string can contain these elements:

  %t stands for the title.
  %s stands for the subtitle.
  %a stands for the author's name.
  %e stands for the author's email.
  %d stands for the date.
  %c will be replaced by `+jg-org-html-creator-string'.
  %v will be replaced by `+jg-org-html-validation-link'.
  %T will be replaced by the export time.
  %C will be replaced by the last modification time.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'jg-org-export-html
  :type '(repeat
	  (list (string :tag "Language")
		(string :tag "Format string"))))

(defcustom +jg-org-html-validation-link
  "<a href=\"https://validator.w3.org/check?uri=referer\">Validate</a>"
  "Link to HTML validation service."
  :group 'jg-org-export-html
  :package-version '(Org . "9.4")
  :type 'string)

(defcustom +jg-org-html-creator-string
  (format "<a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> %s (<a href=\"https://orgmode.org\">Org</a> mode %s)"
	  emacs-version
	  (if (fboundp 'org-version) (org-version) "unknown version"))
  "Information about the creator of the HTML document.
This option can also be set on with the CREATOR keyword."
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(string :tag "Creator string"))

;;-- end postamble

;;-- preamble
(defcustom +jg-org-html-preamble t
  "Non-nil means insert a preamble in HTML export.

When t, insert a string as defined by the formatting string in
`+jg-org-html-preamble-format'.  When set to a string, use this
formatting string instead (see `+jg-org-html-postamble-format' for an
example of such a formatting string).

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting :html-preamble in publishing projects will take
precedence over this variable."
  :group 'jg-org-export-html
  :type '(choice (const :tag "No preamble" nil)
		 (const :tag "Default preamble" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom +jg-org-html-preamble-format '(("en" ""))
  "Alist of languages and format strings for the HTML preamble.

The first element of each list is the language code, as used for
the LANGUAGE keyword.  See `org-export-default-language'.

The second element of each list is a format string to format the
preamble itself.  This format string can contain these elements:

  %t stands for the title.
  %s stands for the subtitle.
  %a stands for the author's name.
  %e stands for the author's email.
  %d stands for the date.
  %c will be replaced by `+jg-org-html-creator-string'.
  %v will be replaced by `+jg-org-html-validation-link'.
  %T will be replaced by the export time.
  %C will be replaced by the last modification time.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\".

See the default value of `+jg-org-html-postamble-format' for an
example."
  :group 'jg-org-export-html
  :type '(repeat
	  (list (string :tag "Language")
		(string :tag "Format string"))))

(defcustom +jg-org-html-link-up ""
  "Where should the \"UP\" link of exported HTML pages lead?"
  :group 'jg-org-export-html
  :type '(string :tag "File or URL"))

(defcustom +jg-org-html-link-home ""
  "Where should the \"HOME\" link of exported HTML pages lead?"
  :group 'jg-org-export-html
  :type '(string :tag "File or URL"))

(defcustom +jg-org-html-link-use-abs-url nil
  "Should we prepend relative links with HTML_LINK_HOME?"
  :group 'jg-org-export-html
  :version "24.4"
  :package-version '(Org . "8.1")
  :type 'boolean)

(defcustom +jg-org-html-home/up-format
  "<div id=\"org-div-home-and-up\">
 <a accesskey=\"h\" href=\"%s\"> UP </a>
 |
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
  "Snippet used to insert the HOME and UP links.
This is a format string, the first %s will receive the UP link,
the second the HOME link.  If both `+jg-org-html-link-up' and
`+jg-org-html-link-home' are empty, the entire snippet will be
ignored."
  :group 'jg-org-export-html
  :type 'string)

;;-- end preamble

;;;; Todos

(defcustom +jg-org-html-todo-kwd-class-prefix ""
  "Prefix to class names for TODO keywords.
Each TODO keyword gets a class given by the keyword itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'jg-org-export-html
  :type 'string)

(provide '+ox-html-jg-vars)
