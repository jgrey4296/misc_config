;;; jg-repl.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +eval--ensure-in-repl-buffer (&optional fn plist displayfn)
  (maphash (lambda (key buffer)
             (unless (buffer-live-p buffer)
               (remhash key +eval-repl-buffers)))
           +eval-repl-buffers)
  (let* ((project-root (doom-project-root))
         (key (cons major-mode project-root))
         (buffer (gethash key +eval-repl-buffers)))
    (cl-check-type buffer (or buffer null))
    (unless (or (eq buffer (current-buffer))
                (null fn))
      (setq buffer
            (funcall (or displayfn #'get-buffer-create)
                     (if (buffer-live-p buffer)
                         buffer
                       (setq buffer
                             (save-window-excursion
                               (if (commandp fn)
                                   (call-interactively fn)
                                 (funcall fn))))
                       (cond ((null buffer)
                              (error "REPL handler %S couldn't open the REPL buffer" fn))
                             ((not (bufferp buffer))
                              (error "REPL handler %S failed to return a buffer" fn)))
                       (with-current-buffer buffer
                         (when plist
                           (setq +eval-repl-plist plist))
                         (+eval-repl-mode +1))
                       (puthash key buffer +eval-repl-buffers)
                       buffer))))
    (when (bufferp buffer)
      (with-current-buffer buffer
        (unless (or (derived-mode-p 'term-mode)
                    (eq (current-local-map) (bound-and-true-p term-raw-map)))
          (goto-char (if (and (derived-mode-p 'comint-mode)
                              (cdr comint-last-prompt))
                         (cdr comint-last-prompt)
                       (point-max)))))
      buffer)))

;;;###autoload
(defun +jg-eval-repl-active-p (&optional handler) ;; -> bool
  "check a repl is active "
  (and (process-live-p (get-buffer-process +eval-repl-buffer-name))
       (or (not handler)
           (and (local-variable-p 'repl-base (get-buffer +eval-repl-buffer-name))
                (repl-handler-p handler)
                (-contains? (ensure-list (repl-handler-modes handler))
                            (with-current-buffer (get-buffer +eval-repl-buffer-name) repl-base)))
           )
       )
  )

;;;###autoload
(defun +jg-eval-determine-repl () ;; -> repl-entry
  "determine the appropriate repl to use for the major mode"
  (let* ((handler (alist-get major-mode +eval-repls))
         )
    (unless (or (not handler) (repl-handler-p handler))
      (error "Did not find a repl-handler for: %s" major-mode))
    handler
    )
  )

;;;###autoload
(defun +jg-eval-open-repl (&optional arg)
  (interactive "P")
  ;; get or choose repl
  (let* ((buff (get-buffer +eval-repl-buffer-name)))

    (when (and arg buff)
      (kill-buffer buff))

    (unless (and buff (buffer-live-p buff))
      (save-window-excursion
        (+jg-eval-start-repl arg)))


    (display-buffer +eval-repl-buffer-name)
    )
  )

;;;###autoload
(defun +jg-eval-start-repl (&optional arg)
  " determine the appropriate repl, start it or open the buffer its already running in.
    if arg, then explicitly choose a repl
 "
  (interactive "P")
  ;; get or choose repl
  (let* ((handler (+jg-eval-determine-repl))
         (buff (get-buffer +eval-repl-buffer-name))
         )
    (when (and arg buff)
      (kill-buffer buff)
      )
    (when arg
      (setq handler (alist-get (intern (ivy-read "Select Repl: " +eval-repls :require-match t)) +eval-repls))
      )

    ;; kill buffer if its dead or incorrect
    (when (and buff (not (+jg-eval-repl-active-p handler)))
      (kill-buffer buff))

    ;; start the repl if neceesary
    (unless (and (repl-handler-p handler) buff (buffer-live-p buff))
      (setq buff (apply (repl-handler-start handler) (list +eval-repl-buffer-name))))

    (when (processp buff)
      (setq buff (process-buffer buff)))

    (unless (bufferp (get-buffer (or buff +eval-repl-buffer-name)))
      (error "Could not get a returned repl buffer: %s" buff))

    ;; store some metadata
    (with-current-buffer (get-buffer (or buff +eval-repl-buffer-name))
      (rename-buffer +eval-repl-buffer-name)
      (setq-local repl-base (car (ensure-list (repl-handler-modes handler))))
      )

    ;; Cleanup
    (unless (and handler (+jg-eval-repl-active-p handler))
      (user-error "Repl couldn't be started: %s" handler))

    )
  )

;;;###autoload
(defun +jg-eval-send-to-repl (beg end &optional str)
  (interactive)
  ;; get or choose repl
  (unless (+jg-eval-repl-active-p)
    (error "no REPL open"))
  (let* ((repl (+jg-eval-determine-repl))
         (buff (get-buffer +eval-repl-buffer-name))
         )
    (user-error "TODO")
    ;; check repl is active
    ;; format selection
    ;; insert into repl
    ;; call RET
    ;; (sit-for 0.001)
    ;; (redisplay 'force)
    )
  )

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
(defun +jg-eval-repl-process-name () ;; maybe[str]
  (-when-let* ((buff (get-buffer +eval-repl-buffer-name))
               (proc (get-buffer-process buff))
               (name (process-name proc))
               )
    name
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    June 11, 2024
;; Modified:   June 11, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; jg-repl.el ends here
