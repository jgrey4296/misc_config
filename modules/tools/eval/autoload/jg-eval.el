;;; jg-eval.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;; adapated from doom's eval
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-eval-repl-active-p () ;; -> bool
  "check a repl is active "

  )

;;;###autoload
(defun +jg-eval-determine-repl (&optional arg) ;; -> repl-entry
  "determine the appropriate repl to use"
  (let* ((repl-by-mode (alist-get major-mode +eval-repls))
         (repl-by-use (if (or arg (not repl-by-mode))
                          (alist-get (intern (ivy-read "Select Repl: " +eval-repls :require-match t)) +eval-repls)))
         (repl-entry (or repl-by-use repl-by-mode))
         )

    (unless repl-entry
      (error "No Repl Found"))
    (message "Found Repl: %s" repl-entry)
    repl-entry
    )
  )

;;;###autoload
(defun +jg-eval-start-repl (&optional arg)
  " determine the appropriate repl, start it or open the buffer its already running in.
    if arg, then explicitly choose a repl
 "
  (interactive "P")
  ;; get or choose repl
  (let* ((repl (+jg-eval-determine-repl arg))
         (buff (get-buffer +eval-repl-buffer-name))
         )

    ;; kill buffer if its dead
    (when (and buff (not (buffer-live-p buff)))
      (kill-buffer buff))

    ;; kill buffer if its the wrong repl
    (when (and buff
               (not (or (eq major-mode (buffer-local-value 'repl-base buff))
                        (eq (plist-get repl-entry :name) (buffer-local-value 'repl-base buff))))
               )
      (kill-buffer buff))

    ;; jump to repl or start it
    (if (and buff (buffer-live-p buff))
        (pop-to-buffer buff)
      (setq buff (funcall (plist-get repl-entry :start))))

    ;; Cleanup
    (if (not (bufferp buff))
        (error (format "Repl call needs to return a buffer: %s" (plist-get repl-entry :start)))
      (with-current-buffer (rename-buffer +eval-repl-buffer-name)
        (setq-local repl-base (or (plist-get repl-entry :name) major-mode)))
      )
    )
  )

;;;###autoload
(defun +jg-eval-send-to-repl (beg end &optional str)
  (interactive)
  ;; get or choose repl
  (unless (+jg-eval-repl-active-p)
    (error "no REPL open"))
  (let* ((repl (+jg-eval-determine-repl arg))
         (buff (get-buffer +eval-repl-buffer-name))
         )
    ;; check repl is active
    ;; format selection
    ;; insert into repl
    ;; call RET
    ;; (sit-for 0.001)
    ;; (redisplay 'force)

    )
  )

;;;###autoload
(defun +jg-eval-run-region-or-str (beg end &optional str)
  (interactive)
  ;; TODO

  )

;;;###autoload
(defun +jg-eval-run-buffer ()
  (interactive)
  (+jg-eval-run-region-or-str (point-min) (point-max))
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
