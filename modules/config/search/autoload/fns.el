;;; config/default/autoload/search.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (read-directory-name "Search directory: ")
            default-directory)))
    (call-interactively #'+ivy/project-search-from-cwd)
    )
  )

;;;###autoload
(defun +default/search-emacsd ()
  "Conduct a text search in files under `doom-emacs-dir'."
  (interactive)
  (let ((default-directory doom-emacs-dir))
    (call-interactively #'+ivy/project-search-from-cwd)
    )
  )

;;;###autoload
(defun +default/search-buffer ()
  "Conduct a text search on the current buffer.

If a selection is active and multi-line, perform a search restricted to that
region.

If a selection is active and not multi-line, use the selection as the initial
input and search the whole buffer for it."
  (interactive)
  (let (start end multiline-p)
    (save-restriction
      (when (region-active-p)
        (setq start (region-beginning)
              end   (region-end)
              multiline-p (/= (line-number-at-pos start)
                              (line-number-at-pos end)))
        (deactivate-mark)
        (when multiline-p (narrow-to-region start end)))
      (call-interactively (if (and start end (not multiline-p))
                              #'swiper-isearch-thing-at-point
                            #'swiper-isearch)))
    )
  )

;;;###autoload
(defun +default/search-notes-for-symbol-at-point (symbol)
  "Conduct a text search in the current project for symbol at point. If prefix
ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (doom-thing-at-point-or-region) ""))))
  (require 'org)
  (+default/search-project-for-symbol-at-point
   symbol org-directory))

;;;###autoload
(defun +default/org-notes-search (query)
  "Perform a text search on `org-directory'."
  (interactive
   (list (if (doom-region-active-p)
             (buffer-substring-no-properties
              (doom-region-beginning)
              (doom-region-end))
           "")))
  (require 'org)
  (+default/search-project-for-symbol-at-point
   query org-directory))

;;;###autoload
(defun +default/org-notes-headlines ()
  "Jump to an Org headline in `org-agenda-files'."
  (interactive)
  (doom-completing-read-org-headings
   "Jump to org headline: " org-agenda-files
   :depth 3
   :include-files t))
