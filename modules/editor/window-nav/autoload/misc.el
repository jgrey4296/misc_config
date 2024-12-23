;;; misc.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defvar jg-window-temp-l "*temp-L*")
(defvar jg-window-temp-r "*temp-R*")

;;;###autoload
(defun +jg-windows-3-col-centered ()
  "Make the current buffer the center of 3, with 2 temp buffers "
  (interactive)
  (delete-other-windows)
  (let* ((curr (current-buffer))
         (left (selected-window))
         (mid (split-window-right))
         (right (with-selected-window mid
                  (split-window-right)))
        )
    (balance-windows)
    (with-selected-window left
      (set-window-buffer left (get-buffer-create jg-window-temp-l))
      )
    (with-selected-window  right
      (set-window-buffer right (get-buffer-create jg-window-temp-r))
      )
    (with-selected-window (select-window mid)
      (solaire-mode 1)
      )
    (with-current-buffer (get-buffer jg-window-temp-l) (solaire-mode -1))
    (with-current-buffer (get-buffer jg-window-temp-r) (solaire-mode -1))
    )
  )

;;;###autoload
(defun +jg-windows-toggle-dedicated ()
  "Make this window dedicated to this buffer"
  (interactive)
  (message "Window Dedication: %s to: %s"
           (set-window-dedicated-p (selected-window)
                                   (not (window-dedicated-p (selected-window))))
           (window-buffer (selected-window))
           )
  )

;;;###autoload
(defun +jg-windows-expand-window (amt)
  "Shrink windows other than the current by amt horizontally"
  (interactive "NExpand By: ")
  (let ((curr (selected-window)))
    (walk-windows #'(lambda (wind)
                      (when (not (eq wind curr))
                        (with-selected-window wind (shrink-window-horizontally amt)))))))

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 14, 2024
;; Modified:   April 14, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; misc.el ends here
