;;; +repl-commands.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <johngrey4296 at gmail.com>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 20, 2022
;; Modified: March 20, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/johngrey/+repl-commands
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;;###autoload
(defun +jg-repl-clear ()
  (interactive)
  (if (+eval--ensure-in-repl-buffer)
      (with-current-buffer (+eval--ensure-in-repl-buffer)
        (+jg-bindings-clear-buffer))
      (message "No Repl to Clear")
      )
  )

;;;###autoload
(defun +jg-repl-send-register-to-repl (reg)
  (interactive "c")
  (let ((str (get-register reg)))
    (message "Got Reg: %s" str)
    (comint-send-string str)
    )
  )

;;;###autoload
(defun +jg-send-region-to-repl (beg end &optional inhibit-auto-execute-p)
  "Execute the selected region in the REPL.
Opens a REPL if one isn't already open. If AUTO-EXECUTE-P, then execute it
immediately after."
  (interactive "rP")
  (unless (+eval--ensure-in-repl-buffer)
    (error "No REPL open"))
  (let* ((origin-window (selected-window))
         (selection (buffer-substring-no-properties beg end))
         (buffer (+eval--ensure-in-repl-buffer))
         (selection-formatted
          (with-temp-buffer
            (insert selection)
            (goto-char (point-min))
            (when (> (skip-chars-forward "\n") 0)
              (delete-region (point-min) (point)))
            (indent-rigidly (point) (point-max)
                            (- (skip-chars-forward " \t")))
            (concat (string-trim-right (buffer-string))
                    "\n"))))
    (if (not (get-buffer-window buffer))
        (+popup-buffer buffer))
    (with-selected-window (get-buffer-window buffer)
      (with-current-buffer buffer
        (dolist (line (split-string selection-formatted "\n"))
          (insert line)
          (if inhibit-auto-execute-p
              (insert "\n")
            ;; Can't use `comint-send-input' b/c there's no guarantee the
            ;; current REPL uses comint. Even if it did, no telling if they
            ;; have their own `comint-send-input' wrapper, so to be safe, I
            ;; simply emulate the keypress.
            (call-interactively (doom-lookup-key (kbd "RET"))))
          (sit-for 0.001)
          (redisplay 'force)))
      (when (and (eq origin-window (selected-window))
                 (bound-and-true-p evil-local-mode))
        (call-interactively #'evil-append-line)))
    ))

;;;###autoload
(defun +jg-eval-open-repl (&optional arg)
  (interactive "P")
  ;; get or choose repl
  (let* ((repl-by-mode (alist-get major-mode +eval-repls))
         (repl-by-use (if (or arg (not repl-by-mode))
                          (alist-get (intern (ivy-read "Select Repl: " +eval-repls :require-match t)) +eval-repls)))
         (repl-entry (or repl-by-use repl-by-mode))
         (buff (get-buffer +eval-repl-buffer-name))
         )

    (unless repl-entry
      (error "No Repl Found"))
    (message "Found Repl: %s" repl-entry)

    (when (and buff (not (buffer-live-p buff)))
      (kill-buffer buff))
    (when (and buff (not (or (eq major-mode (buffer-local-value 'repl-base buff))
                             (eq (plist-get repl-entry :name) (buffer-local-value 'repl-base buff)))))
      (kill-buffer buff))

    (if (and buff (buffer-live-p buff))
        (pop-to-buffer buff)
      ;; if repl exists already, jump to it
      (setq buff (funcall (plist-get repl-entry :start))))

    (if (not (bufferp buff))
        (error (format "Repl call needs to return a buffer: %s" (plist-get repl-entry :start)))
      (with-current-buffer (rename-buffer +eval-repl-buffer-name)
        (setq-local repl-base (or (plist-get repl-entry :name)
                                  major-mode)))
      )
    )
  )

;;; +repl-commands.el ends here
