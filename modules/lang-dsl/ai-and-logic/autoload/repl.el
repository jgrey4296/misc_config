;;; +repl.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ceptre-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'ceptre-mode)
  (if (not (bufferp ceptre-repl-buffer-name))
      (make-comint-in-buffer ceptre-repl-buffer-name
                             (get-buffer-create ceptre-repl-buffer-name)
                             ceptre-executable))
  (get-buffer-create ceptre-repl-buffer-name)
  )

;;;###autoload
(defun +soar-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'soar-mode)
  (if (not (bufferp soar-comint-buffer-name))
      (make-comint-in-buffer soar-comint-buffer-name
                             (get-buffer-create soar-comint-buffer-name)
                             soar-executable))
  (get-buffer-create soar-comint-buffer-name)
  )

;;;###autoload
(defun +clips-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'clips-mode)
  (if (not (bufferp inferior-clips-buffer))
      (run-clips))
  (get-buffer-create inferior-clips-buffer)
  )

;;;###autoload
(defun +instal-mode/open-repl (&optional arg)
  (interactive "P")
  (require 'instal-mode)
  (if (not (bufferp instal-repl-buffer-name))
      (make-comint-in-buffer instal-repl-buffer-name
                             (get-buffer-create instal-repl-buffer-name)
                             instal-executable))
  (get-buffer-create instal-repl-buffer-name)
  )
