;;; util.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun +ivy-rich-buffer-name (candidate)
  "Display the buffer name.

Buffers that are considered unreal (see `doom-real-buffer-p') are dimmed with
`+ivy-buffer-unreal-face'."
  (let ((b (get-buffer candidate)))
    (when (null uniquify-buffer-name-style)
      (setq candidate (replace-regexp-in-string "<[0-9]+>$" "" candidate)))
    (cond ((ignore-errors
             (file-remote-p
              (with-current-buffer b default-directory)))
           (ivy-append-face candidate 'ivy-remote))
          ((doom-unreal-buffer-p b)
           (ivy-append-face candidate +ivy-buffer-unreal-face))
          ((not (buffer-file-name b))
           (ivy-append-face candidate 'ivy-subdir))
          ((buffer-modified-p b)
           (ivy-append-face candidate 'ivy-modified-buffer))
          (candidate))))

;;;###autoload
(defun +ivy--switch-buffer-preview ()
  (let (ivy-use-virtual-buffers ivy--virtual-buffers)
    (counsel--switch-buffer-update-fn)))

;;;###autoload
(cl-defun +ivy-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg") (user-error "Couldn't find ripgrep in your PATH"))
  (require 'counsel)
  (let* ((this-command 'counsel-rg)
         (project-root (or (projectile-project-root) default-directory))
         (directory (or in project-root))
         (args (concat (if all-files " -uu")
                       (unless recursive " --maxdepth 1")
                       " --hidden -g!.git "
                       (mapconcat #'shell-quote-argument args " "))))
    (setq deactivate-mark t)
    (counsel-rg
     (or query
         (when (doom-region-active-p)
           (replace-regexp-in-string
            "[! |]" (lambda (substr)
                      (cond ((and (string= substr " ")
                                  (not (modulep! +fuzzy)))
                             "  ")
                            ((string= substr "|")
                             "\\\\\\\\|")
                            ((concat "\\\\" substr))))
            (rxt-quote-pcre (doom-thing-at-point-or-region)))))
     directory args
     (or prompt
         (format "Search project [%s]: "
                 (cond ((equal directory default-directory)
                        "./")
                       ((equal directory project-root)
                        (projectile-project-name))
                       ((file-relative-name directory project-root)))
                 (string-trim args))))))

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    March 22, 2025
;; Modified:   March 22, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; util.el ends here
