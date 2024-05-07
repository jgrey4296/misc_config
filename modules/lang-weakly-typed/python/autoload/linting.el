;;; linting.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-python-ruff-format ()
  " Run ruff format on a temp ver of the current buffer
then run ediff on the result against the original
"
  (interactive)
  (basic-save-buffer)
  (let* ((current-file (buffer-file-name))
         (current      (current-buffer))
         (temp-file (make-temp-file (format "ruff-%s" (buffer-name)) nil ".py" (buffer-substring-no-properties (point-min) (point-max))))
         (fmt-process (start-process "ruff" "*ruff-fmt*" "ruff" "format" temp-file))
         )
    (message "Setting Sentinel with: %s : %s" current temp-file)
    (set-process-sentinel fmt-process
                          (-partial #'(lambda (curr temp proc x)
                                        (when (not (process-live-p proc))
                                          (ediff-files (buffer-file-name curr) temp)
                                          )
                                        )
                                    current temp-file
                                    )
                          )
    )
  )

;;;###autoload
(defun +jg-python-isort-diff ()
  " Run isort on a temp ver of the current buffer
then run ediff on the result against the original
"
  (interactive)
  (basic-save-buffer)
  (let* ((current-file (buffer-file-name))
         (current      (current-buffer))
         (temp-file    (make-temp-file (format "isort-%s" (buffer-name)) nil ".py" (buffer-substring-no-properties (point-min) (point-max))))
         (isort-process (start-process "isort" "*isort*" "isort" temp-file (concat "--settings-path=" (projectile-project-root))))
         )
    (message "Setting Sentinel with: %s : %s" current temp-file)
    (set-process-sentinel isort-process
                          (-partial #'(lambda (curr temp proc x)
                                        (when (not (process-live-p proc))
                                          (ediff-files (buffer-file-name curr) temp)
                                          )
                                        )
                                    current temp-file
                                    )
                          )
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 30, 2024
;; Modified:   April 30, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; linting.el ends here
