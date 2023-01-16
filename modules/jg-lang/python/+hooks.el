;;; +hooks.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <johngrey4296 at gmail.com>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 05, 2022
;; Modified: April 05, 2022
;; Version: 0.0.1
;; Homepage: https://github.com/johngrey/+hooks
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun +jg-python-outline-regexp-override-hook ()
  (setq-local outline-regexp jg-python-outline-regexp
              outline-heading-end-regexp jg-python-outline-end-regexp
              outline-level #'+jg-python-outline-level
              )
  )

(define-advice +python-use-correct-flycheck-executables-h (:override ()
                                                           +jg-python-flycheck-override)
  "Use the correct Python executables for Flycheck."
  (let ((executable python-shell-interpreter))
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                  (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
          (setq executable (substring-no-properties (match-string 1))))))
    ;; Try to compile using the appropriate version of Python for
    ;; the file.
    (setq-local flycheck-python-pycompile-executable executable)
    ;; We might be running inside a virtualenv, in which case the
    ;; modules won't be available. But calling the executables
    ;; directly will work.
    (setq-local flycheck-python-pylint-executable "pylint")
    (setq-local flycheck-python-flake8-executable "flake8")))

(defun +jg-python-auto-kill-anaconda-processes-h ()
    "Kill anaconda processes if this buffer is the last python buffer."
    (when (and (eq major-mode 'python-mode)
               (not (delq (current-buffer)
                          (doom-buffers-in-mode 'python-mode (buffer-list)))))
      (progn (message "Killing Ananaconda")
             (anaconda-mode-stop))))

(defun +jg-python-auto-kill-conda-hook ()
  (add-hook 'kill-buffer-hook
            #'+jg-python-auto-kill-anaconda-processes-h
            nil 'local)
  )

(defun +jg-python-auto-hide ()
  " Add auto-hiding to python.
Hides imports in a vimish fold,
Add any sections commented with jg-python-fold-block-[start|end]-re
and closes classes and functions, re-opening only the first class "
  (when autohide-minor-mode
    (message "Running Python Auto Hide")
    (save-excursion
      (beginning-of-buffer)
      ;; Fold Imports
      (message "Searching for import block")
      (evil-close-folds)
      (when (re-search-forward "^class " nil t)
        (let ((current-prefix-arg t))
          (evil-close-fold))
        )
      )
    )
  )

;; (defun +jg-python-def-bounds ()
;;   (interactive)
;;   (cons (py--end-of-def-or-class-position)
;;         (py--beginning-of-def-or-class-position))
;;   )

(defun +jg-python-customisation-hook ()
  ;; (put 'defun 'bounds-of-thing-at-point '+jg-python-def-bounds)
  (setq-local
   end-of-defun-function       'python-nav-end-of-defun
   beginning-of-defun-function 'python-nav-beginning-of-defun
   indent-region-function      'python-indent-region
   indent-line-function        'python-indent-line
   )

  (add-hook 'jg-text-whitespace-clean-hook '+jg-python-cleanup-ensure-newline-before-def 5 t)
  (add-hook 'jg-text-whitespace-clean-hook 'delete-trailing-whitespace 10 t)
  (add-hook 'jg-text-whitespace-clean-hook '+jg-python-cleanup-whitespace 20 t)
  )
;;; +hooks.el ends here
