;;; jg-eval.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;; adapated from doom's eval
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +eval-display-results-in-popup (output &optional _source-buffer)
  "Display OUTPUT in a popup buffer."
  (let ((output-buffer (get-buffer-create "*doom eval*"))
        (origin (selected-window)))
    (with-current-buffer output-buffer
      (setq-local scroll-margin 0)
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (if (fboundp '+word-wrap-mode)
          (+word-wrap-mode +1))
    (when-let (win (display-buffer output-buffer))
      (fit-window-to-buffer
       win (/ (frame-height) 2)
       nil (/ (frame-width) 2)))
    (select-window origin)
    output-buffer))
  )

;;;###autoload
(defun +eval-display-results-in-overlay (output &optional source-buffer)
  "Display OUTPUT in a floating overlay next to the cursor."
  (require 'eros)
  (let* ((this-command #'+eval/buffer-or-region)
         (prefix eros-eval-result-prefix)
         (lines (split-string output "\n"))
         (prefixlen (length prefix))
         (len (+ (apply #'max (mapcar #'length lines))
                 prefixlen))
         (col (- (current-column) (window-hscroll)))
         (next-line? (or (cdr lines)
                         (< (- (window-width)
                               (save-excursion (goto-char (point-at-eol))
                                               (- (current-column)
                                                  (window-hscroll))))
                            len)))
         (pad (if next-line?
                  (+ (window-hscroll) prefixlen)
                0))
         (where (if next-line?
                    (line-beginning-position 2)
                  (line-end-position)))
         eros-eval-result-prefix
         eros-overlays-use-font-lock)
    (with-current-buffer (or source-buffer (current-buffer))
      (eros--make-result-overlay
          (concat (make-string (max 0 (- pad prefixlen)) ?\s)
                  prefix
                  (string-join lines (concat "\n" (make-string pad ?\s))))
        :where where
        :duration eros-eval-result-duration))))

;;;###autoload
(defun +eval-display-results (output &optional source-buffer)
  "Display OUTPUT in an overlay or a popup buffer."
  (funcall (if (or current-prefix-arg
                   (with-temp-buffer
                     (insert output)
                     (or (>= (count-lines (point-min) (point-max))
                             +eval-popup-min-lines)
                         (>= (string-width
                              (buffer-substring (point-min)
                                                (save-excursion
                                                  (goto-char (point-min))
                                                  (line-end-position))))
                             (window-width))))
                   (not (require 'eros nil t)))
               #'+eval-display-results-in-popup
             #'+eval-display-results-in-overlay)
           output source-buffer)
  output)

(defvar quickrun-option-cmdkey)


;;;###autoload
(defun +jg-eval-run-region-or-str (beg end &optional str)
  (interactive)

  )

;;;###autoload
(defun +jg-eval-run-buffer ()
  "Evaluate the whole buffer."
  (interactive)
  (let ((handler (alist-get major-mode +eval-handlers))
        )
    (cond ((not (eval-handler-p handler))
           (warn "Could not find an eval handler for: %s" major-mode)
           )
          ((eval-handler-fn handler)
           (funcall (eval-handler-fn handler) (point-min) (point-max))
           )
          ((eval-handler-indirect handler)
           (funcall (eval-handler-indirect handler) (point-min) (point-max))
           )
          (t
           (warn "Could not execute a handler: %s" handler)
           )
          )
    )
)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 03, 2024
;; Modified:   April 03, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; jg-eval.el ends here
