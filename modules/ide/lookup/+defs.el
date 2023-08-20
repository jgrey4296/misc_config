;;; +defs.el -*- lexical-binding: t; -*-


(defvar +lookup-provider-url-alist nil
  "An alist that maps online resources to either:

  1. A search url (needs on '%s' to substitute with an url encoded query),
  2. A non-interactive function that returns the search url in #1,
  3. An interactive command that does its own search for that provider.

Used by `+lookup/online'.")

(defvar +lookup-open-url-fn #'browse-url
  "Function to use to open search urls.")

(defvar +lookup-definition-defaults
  '(+lookup-dictionary-definition-backend-fn
    +lookup-xref-definitions-backend-fn
    +lookup-dumb-jump-backend-fn
    +lookup-project-search-backend-fn
    +lookup-evil-goto-definition-backend-fn)
)

(defvar +lookup-declaration-defaults nil)

(defvar +lookup-implementations-defaults ())

(defvar +lookup-type-definition-defaults ())

(defvar +lookup-references-defaults '(+lookup-thesaurus-definition-backend-fn
                                      +lookup-xref-references-backend-fn
                                      +lookup-project-search-backend-fn)
  )

(defvar +lookup-documentation-defaults '(+lookup-dash-docsets-backend-fn +lookup-online-backend-fn))

(defvar +lookup-file-defaults '(+lookup-bug-reference-backend-fn +lookup-ffap-backend-fn))

(defvar +lookup-dictionary-prefer-offline nil)
