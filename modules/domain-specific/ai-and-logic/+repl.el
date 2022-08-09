;;; +repl.el -*- lexical-binding: t; -*-

(message "Setting up ai and logic repls")

(defun +ceptre-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'ceptre-mode)
  (if (not (bufferp ceptre-repl-buffer-name))
      (make-comint-in-buffer ceptre-repl-buffer-name
                             (get-buffer-create ceptre-repl-buffer-name)
                             ceptre-executable))
  (get-buffer-create ceptre-repl-buffer-name)
  )

(defun +soar-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'soar-mode)
  (if (not (bufferp soar-comint-buffer-name))
      (make-comint-in-buffer soar-comint-buffer-name
                             (get-buffer-create soar-comint-buffer-name)
                             soar-executable))
  (get-buffer-create soar-comint-buffer-name)
  )

(defun +clips-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'clips-mode)
  (if (not (bufferp inferior-clips-buffer))
      (run-clips))
  (get-buffer-create inferior-clips-buffer)
  )

(defun +instal-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'instal-mode)
  (if (not (bufferp instal-repl-buffer-name))
      (make-comint-in-buffer instal-repl-buffer-name
                             (get-buffer-create instal-repl-buffer-name)
                             instal-executable))
  (get-buffer-create instal-repl-buffer-name)
  )

(set-repl-handler! 'clips-mode  '+clips-mode/open-repl)
(set-repl-handler! 'instal-mode '+instal-mode/open-repl)
(set-repl-handler! 'soar-mode   '+soar-mode/open-repl)
(set-repl-handler! 'ceptre-mode '+ceptre-mode/open-repl)
