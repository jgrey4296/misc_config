;;; ivy.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-evil-state-ivy ()
  (interactive)
  (ivy-read "Evil State: "
            (mapcar #'car evil-state-properties)
            :require-match t
            :def 'normal
            :sort t
            :action #'(lambda (x)
                        (let ((val (intern-soft (format "evil-%s-state" x))))
                          (when (fboundp val) (funcall val))))
            )
)


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 25, 2024
;; Modified:   July 25, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; ivy.el ends here
