;;; cookiecutter.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-dired-cookiecutter ()
  (interactive)
  (let* ((current (selected-window))
         (templates (with-temp-buffer
                      (call-process "cookiecutter" nil t nil "-l")
                      (mapcar (-partial #'s-chop-prefix " * ") (cdr (s-split "\n" (buffer-string))))))
         (template (ivy-read "Cookiecutter Templates: " templates :require-match t))
         (buffer (get-buffer-create "*cookiecutter*"))
         )
    (make-comint-in-buffer "cookiecutter" buffer "cookiecutter" nil template)
    (set-process-sentinel (get-buffer-process buffer)
                          (-partial #'(lambda (wind proc x)
                                        (when (not (process-live-p proc))
                                          (select-window wind))
                                        )
                                    current))
    (pop-to-buffer buffer)
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    March 24, 2024
;; Modified:   March 24, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; cookiecutter.el ends here
