;; +defs.el -*- lexical-binding: t; -*-

;; Reduce default verbosity. 3 is too chatty about initializing yasnippet. 2
;; is just right (only shows errors).

(defvar jg-snippets-code-templates-dir    (expand-file-name "code" templates-loc))

(defvar jg-snippets-file-templates-dir    (expand-file-name "files" templates-loc))

(defvar jg-snippets-project-templates-dir (expand-file-name "projects" templates-loc))

(defvar yas-verbosity 2)

(defvar +snippets--smartparens-enabled-p t)

(defvar +snippets--expanding-p nil)

(defvar +snippets-dir       jg-snippets-code-templates-dir)

(defvar jg-snippet-dirs nil)


(defvar +file-templates-default-trigger "__")

(defvar +file-templates-inhibit nil)

(defvar +file-templates-dir jg-snippets-file-templates-dir)

(defvar +file-templates-alist nil)
