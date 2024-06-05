;;; +vars.el -*- lexical-binding: t; -*-

;;-- locations

(defvar +org-capture-todo-file nil
  "Default target for todo entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-changelog-file nil
  "Default target for changelog entries.

Is relative to `org-directory' unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-notes-file nil
  "Default target for storing notes.

Used as a fall back file for org-capture.el, for templates that do not specify a
target file.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-journal-file nil
  "Default target for storing timestamped journal entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-projects-file nil
  "Default, centralized target for org-capture templates.")

(defvar jg-org-link-move-base "/Volumes/Overflow/missing_images/")

(defvar jg-org-twitter-loc "/Volumes/documents/twitter_threads/")

;; Set to nil so we can detect user changes to them later (and fall back on
;; defaults otherwise).

(defvar org-directory nil)

(defvar org-id-locations-file nil)

(defvar org-attach-id-dir nil)
;;-- end locations

;;-- babel

(defvar +org-babel-native-async-langs '(python)
  "Languages that will use `ob-comint' instead of `ob-async' for `:async'.")

(defvar +org-babel-mode-alist nil
  "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.

For example, (fish . shell) will cause #+begin_src fish blocks to load
ob-shell.el when executed.")

(defvar org-babel-python-command nil)
;;-- end babel

;;-- habit

(defvar +org-habit-graph-padding 2
  "The padding added to the end of the consistency graph")

(defvar +org-habit-min-width 30
  "Hides the consistency graph if the `org-habit-graph-column' is less than this value")

(defvar +org-habit-graph-window-ratio 0.3
  "The ratio of the consistency graphs relative to the window width")

;;-- end habit

(defvar +org-startup-with-animated-gifs nil
  "If non-nil, and the cursor is over a gif inline-image preview, animate it!")

;; org-crypt falls back to CRYPTKEY property then `epa-file-encrypt-to', which
;; is a better default than the empty string `org-crypt-key' defaults to.

(defvar org-crypt-key nil)

;;-- evil
(defvar evil-org-retain-visual-state-on-shift t)

(defvar evil-org-special-o/O '(table-row))

(defvar evil-org-use-additional-insert t)

;;-- end evil

;; Make most of the default modules opt-in to lighten its first-time load
;; delay. I sincerely doubt most users use them all.

(defvar org-modules '(ol-bibtex))

(defvar jg-org-external-file-link-types '("jpg" "jpeg" "png" "mp4" "html"))

(defvar jg-org-clean-marker nil)

(defvar jg-org-preferred-linecount 1500)

(defvar-local jg-org-startup-agenda nil)
(defvar-local jg-org-startup-reference nil)
(defvar-local jg-org-startup-package nil)
(defvar jg-org-startup-reference-files nil "Files that are used for reference")
(defvar jg-org-startup-package-files nil)
