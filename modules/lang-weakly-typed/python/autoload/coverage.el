;;; coverage.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-python-open-coverage-report ()
  (interactive)
  (let* ((base (projectile-project-root))
         (coverage-dir (expand-file-name jg-python-coverage-file-loc base))
         (report (expand-file-name "html_report/index.html" coverage-dir))
        )
    (when (f-exists? report)
      (call-process "open" nil nil nil report)
      )
    )
  )

;;;###autoload
(defun +jg-python-toggle-coverage-hook ()
  (interactive)
  (if (-contains? python-mode-hook #'python-coverage-overlay-mode)
      (remove-hook 'python-mode-hook #'python-coverage-overlay-mode)
    (add-hook 'python-mode-hook #'python-coverage-overlay-mode)
    )
  )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 20, 2024
;; Modified:   July 20, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; coverage.el ends here
