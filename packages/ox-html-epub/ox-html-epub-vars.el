;;; +ox-html-epub-vars.el -*- lexical-binding: t; -*-

(defgroup org-export-html-epub nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export HTML"
  :group 'org-export)

(defcustom org-html-epub-htmlize-output-type 'css
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
`\\[org-html-epub-htmlize-generate-css]' to extract class definitions."
  :group 'org-export-html-epub
  :type '(choice (const css) (const inline-css) (const nil)))

;;-- footnotes

(defcustom org-html-epub-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'org-export-html-epub
  :type 'string)

;;-- end footnotes

;;-- headlines

(defcustom org-html-epub-format-headline-function
  'org-html-epub-format-headline-default-function
  "Function to format headline text.

This function will be called with six arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags (string or nil).
INFO      the export options (plist).

The function result will be used in the section format string."
  :group 'org-export-html-epub
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'function)

;;-- end headlines

;;-- images

(defcustom org-html-epub-inline-image-rules
  `(("file" . ,(regexp-opt '(".jpeg" ".jpg" ".png" ".gif" ".svg" ".webp")))
    ("http" . ,(regexp-opt '(".jpeg" ".jpg" ".png" ".gif" ".svg" ".webp")))
    ("https" . ,(regexp-opt '(".jpeg" ".jpg" ".png" ".gif" ".svg" ".webp"))))
  "Rules characterizing image files that can be inlined into HTML.
A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path."
  :group 'org-export-html-epub
  :package-version '(Org . "9.5")
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))
(defvar org-html-epub-standalone-image-predicate)
;;-- end images

;;-- html file settings

(defcustom org-html-epub-xml-declaration
  '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>"))
  "The extension for exported HTML files.
%s will be replaced with the charset of the exported file.
This may be a string, or an alist with export extensions
and corresponding declarations.

This declaration only applies when exporting to XHTML."
  :group 'org-export-html-epub
  :type '(choice
	  (string :tag "Single declaration")
	  (repeat :tag "Dependent on extension"
		  (cons (string :tag "Extension")
			(string :tag "Declaration")))))

;;-- end html file settings

;;-- postamble

(defcustom org-html-epub-postamble-format
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
  %c will be replaced by `org-html-epub-creator-string'.
  %v will be replaced by `org-html-epub-validation-link'.
  %T will be replaced by the export time.
  %C will be replaced by the last modification time.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-html-epub
  :type '(repeat
	  (list (string :tag "Language")
		(string :tag "Format string"))))

;;-- end postamble

;;-- preamble

(defcustom org-html-epub-link-up ""
  "Where should the \"UP\" link of exported HTML pages lead?"
  :group 'org-export-html-epub
  :type '(string :tag "File or URL"))

(defcustom org-html-epub-link-use-abs-url nil
  "Should we prepend relative links with HTML_LINK_HOME?"
  :group 'org-export-html-epub
  :version "24.4"
  :package-version '(Org . "8.1")
  :type 'boolean)

(defcustom org-html-epub-home/up-format
  ""
  "Snippet used to insert the HOME and UP links.
This is a format string, the first %s will receive the UP link,
the second the HOME link.  If both `org-html-epub-link-up' and
`org-html-epub-link-home' are empty, the entire snippet will be
ignored."
  :group 'org-export-html-epub
  :type 'string)

;;-- end preamble

;;;; Todos

(provide 'ox-html-epub-vars)
