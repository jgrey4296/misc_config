;;; bytecoder.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header


;;;###autoload
(defun +jg-python-bytecode-dwim ()
  " run python dis on the file, or a function, according to visual state "
  (interactive)
  (with-current-buffer (get-buffer-create "*python-dis*")
    (erase-buffer))
  (cond ((evil-visual-state-p) ;; visual, use a tempfile
         (let* ((selection (buffer-substring-no-properties evil-visual-beginning evil-visual-end))
                (temp (make-temp-file "python-dis-"))
                )
           (with-temp-file temp (insert selection))
           (make-process :name "python-dis"
                         :buffer "*python-dis*"
                         :command (list "python" "-m" "dis" temp)
                         :noquery t
                         :sentinel #'+jg-python-bytecode--sentinel
                         )))
        (t ;; else use entire file
         (make-process :name "python-dis"
                       :buffer "*python-dis*"
                       :command (list "python" "-m" "dis" (buffer-file-name))
                       :noquery t
                       :sentinel #'+jg-python-bytecode--sentinel
          ))
        )
  )

(defun +jg-python-bytecode--sentinel (proc stat)
  (when (not (process-live-p proc))
    (display-buffer "*python-dis*")
    )
  )

;;;###autoload
(defun +jg-python-dired-dis ()
  " asynchronously disassemble each marked file in a directory,
to {filename}.dis"
  (interactive)
  (cl-loop for file in (dired-get-marked-files)
           do
           (let ((name (format "%s-dis" (f-base file)))
                 (dis-file (format "%s.dis" file))
                 )
             (make-process :name name
                           :buffer (format "*%s*" name)
                           :command (list "python" "-m" "dis" file)
                           :noquery t
                           :sentinel (-partial
                                      #'+jg-python-bytecode--sentinel-writer
                                      dis-file)

                           )
             )
           )
  )

(defun +jg-python-bytecode--sentinel-writer (target proc stat)
  (when (not (process-live-p proc))
    (with-current-buffer (process-buffer proc)
      (write-file target)
      )
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 05, 2024
;; Modified:   April 05, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; bytecoder.el ends here
