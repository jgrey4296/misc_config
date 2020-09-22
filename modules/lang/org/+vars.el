;;; lang/org/+vars.el -*- lexical-binding: t; -*-

(defvar +org-babel-mode-alist
  '((cpp . C)
    (C++ . C)
    (D . C)
    (elisp . emacs-lisp)
    (sh . shell)
    (bash . shell)
    (matlab . octave)
    (rust . rustic-babel)
    (amm . ammonite))
  "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.

For example, (fish . shell) will cause #+BEGIN_SRC fish blocks to load
ob-shell.el when executed.")
(defvar +org-babel-load-functions ()
  "A list of functions executed to load the current executing src block. They
take one argument (the language specified in the src block, as a string). Stops
at the first function to return non-nil.")
(defvar +org-capture-todo-file "todo.org"
  "Default target for todo entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")
(defvar +org-capture-changelog-file "changelog.org"
  "Default target for changelog entries.

Is relative to `org-directory' unless it is absolute. Is used in Doom's default
`org-capture-templates'.")
(defvar +org-capture-notes-file "notes.org"
  "Default target for storing notes.

Used as a fall back file for org-capture.el, for templates that do not specify a
target file.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")
(defvar +org-capture-journal-file "journal.org"
  "Default target for storing timestamped journal entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")
(defvar +org-capture-projects-file "projects.org"
  "Default, centralized target for org-capture templates.")
(defvar +org-habit-graph-padding 2
  "The padding added to the end of the consistency graph")
(defvar +org-habit-min-width 30
  "Hides the consistency graph if the `org-habit-graph-column' is less than this value")
(defvar +org-habit-graph-window-ratio 0.3
  "The ratio of the consistency graphs relative to the window width")
