;;; +vars.el -*- lexical-binding: t; -*-


(defvar +org-babel-native-async-langs '(python)
  "Languages that will use `ob-comint' instead of `ob-async' for `:async'.")

(defvar +org-babel-mode-alist
  '((c . C)
    (cpp . C)
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

For example, (fish . shell) will cause #+begin_src fish blocks to load
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

(defvar +org-startup-with-animated-gifs nil
  "If non-nil, and the cursor is over a gif inline-image preview, animate it!")

;; org-crypt falls back to CRYPTKEY property then `epa-file-encrypt-to', which
;; is a better default than the empty string `org-crypt-key' defaults to.
(defvar org-crypt-key nil)

(defvar evil-org-retain-visual-state-on-shift t)
(defvar evil-org-special-o/O '(table-row))
(defvar evil-org-use-additional-insert t)

;; Set to nil so we can detect user changes to them later (and fall back on
;; defaults otherwise).
(defvar org-directory nil)
(defvar org-id-locations-file nil)
(defvar org-attach-id-dir nil)
(defvar org-babel-python-command nil)

(setq org-persist-directory (concat doom-cache-dir "org/persist/")
      org-publish-timestamp-directory (concat doom-cache-dir "org/timestamps/")
      org-preview-latex-image-directory (concat doom-cache-dir "org/latex/")
      ;; Recognize a), A), a., A., etc -- must be set before org is loaded.
      org-list-allow-alphabetical t)

;; Make most of the default modules opt-in to lighten its first-time load
;; delay. I sincerely doubt most users use them all.
(defvar org-modules
  '(;; ol-w3m
    ;; ol-bbdb
    ol-bibtex
    ;; ol-docview
    ;; ol-gnus
    ;; ol-info
    ;; ol-irc
    ;; ol-mhe
    ;; ol-rmail
    ;; ol-eww
    ))
