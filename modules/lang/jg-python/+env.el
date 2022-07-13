;;; +env.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 10, 2022
;; Modified: July 10, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Code for activating environments,
;;  both pvenv and conda
;;
;;; Code:
(defun +jg-python-support ()
  " Toggle dev support from conda, lsp, or nothing "
  (interactive)
  (let ((selection (ivy-read "Python Support Options: " '(lsp conda flycheck pyright pylint nothing)))
        (stop-lsp (lambda ()
                    (remove-hook 'python-mode-local-vars-hook 'lsp!)
                    ;; disconnect each python buffer from the server
                    (if (fboundp 'lsp-disconnect)
                        (mapc (lambda (x) (with-current-buffer x (if (eq major-mode 'python-mode) (lsp-disconnect)))) (buffer-list))
                    )))
        (stop-anaconda (lambda ()
                         (if (fboundp 'anaconda-mode-stop) (anaconda-mode-stop))
                         (if (fboundp 'anaconda-eldoc-mode) (anaconda-eldoc-mode -1))
                         (remove-hook 'python-mode-local-vars-hook 'anaconda-mode)
                         (remove-hook 'python-mode-local-vars-hook 'anaconda-eldoc-mode)
                         (remove-hook 'python-mode-hook '+jg-python-auto-kill-conda-hook)
                         ))
        (stop-flycheck (lambda ()
                         (global-flycheck-mode -1)
                         (flycheck-global-teardown)
                         ))
        )
    (setq jg-python-last-chosen-support selection)
    (cond ((s-equals? selection 'lsp)
           (message "got lsp")
           (apply stop-anaconda nil)
           (add-hook 'python-mode-local-vars-hook 'lsp!)
           (with-current-buffer (current-buffer)
             (if (eq major-mode 'python-mode) (lsp!)))
           )
          ((s-equals? selection 'conda)
           (message "got conda")
           (apply stop-lsp nil)
           (add-hook 'python-mode-hook '+jg-python-auto-kill-conda-hook)
           (add-hook 'python-mode-local-vars-hook 'anaconda-mode)
           (add-hook 'python-mode-local-vars-hook 'anaconda-eldoc-mode)
           (with-current-buffer (current-buffer)
             (if (eq major-mode 'python-mode) (anaconda-mode)))
           )
          ((s-equals? selection 'flycheck)
           (message "got flycheck")
           (apply stop-anaconda nil)
           (apply stop-lsp nil)
           (global-flycheck-mode)
           )
          ((s-equals? selection 'pyright)
           (delete 'python-pyright flycheck-disabled-checkers)
           (apply stop-lsp nil)
           (apply stop-anaconda nil)
           )
          ((s-equals? selection 'pylint)
           (delete 'python-pylint flycheck-disabled-checkers)
           (apply stop-lsp nil)
           (apply stop-anaconda nil)
           )
          ((s-equals? selection 'nothing)
           (message "got nothing")
           (apply stop-lsp nil)
           (apply stop-anaconda nil)
           )
          (t
           (message "Unrecognized option: %s" selection)
           )
          )
    )
  )
(defun find-venv (&optional start)
  " Given a starting directory, look in parent dirs
until a .venv file is found.

return (dir-of-venv env-name) or nil
"
  (let* ((cwd default-directory)
         (root (locate-dominating-file (or start buffer-file-name default-directory) ".venv"))
         env-name)
      (if (and root (f-exists? (f-join root ".venv")))
          (with-temp-buffer
            (insert-file-contents (f-join root".venv"))
            (goto-char (point-min))
            (setq env-name (f-filename (buffer-substring-no-properties (point-min) (line-end-position))))
            )
        (message "Couldn't find a .venv"))

    ;; return nil or (venv-dir env-name)
    (if env-name
        (list root env-name)
      nil)
    )
  )

(defun +jg-python-activate-venv-and-conda (&optional prefix)
  "Dual Activate pyvenv and conda environments"
  (interactive "P")
  (require 'pyvenv)
  (require 'conda)
  (let* ((project (if (not prefix) (find-venv)))
         (env-name (cond ((stringp prefix) prefix)
                         (prefix (conda--read-env-name))
                         (project (cadr project))
                         (t (conda--read-env-name))))
         (env-dir (conda-env-name-to-dir env-name)))
    ;; Activate venv
    (if (not (s-equals? pyvenv-virtual-env-name env-name))
        (pyvenv-activate env-dir))
    ;; activate conda
    (if (not (s-equals? conda-env-current-name env-name))
        (conda-env-activate-path env-dir))

    ;; add project if it exists to extra paths
    (if project
        (setq python-shell-extra-pythonpaths (list (car project))
              lsp-pyright-extra-paths (vector (car project))
              conda-project-env-path env-name)
      ;; else add curr-directory
      (setq python-shell-extra-pythonpaths (list default-directory)
            lsp-pyright-extra-paths (vector default-directory)
            conda-project-env-path env-name))
    )
  (cl-assert (s-equals? pyvenv-virtual-env-name conda-env-current-name))
  )

(define-advice conda--get-path-prefix (:override (env-dir)
                                       jg-conda--get-path-prefix)
  "Get a platform-specific path string to utilize the conda env in ENV-DIR.
It's platform specific in that it uses the platform's native path separator."
  (let* ((conda-anaconda-home-tmp conda-anaconda-home)
         (conda-executable-path
          (concat (file-name-as-directory conda-anaconda-home-tmp)
                  (file-name-as-directory conda-env-executables-dir)
                  "conda"))
         (base-command jg-conda-activate-cmd)
         (command (format base-command env-dir))
         (result
          (with-output-to-string
            (with-current-buffer standard-output
              (unless (= 0 (process-file shell-file-name nil '(t nil) nil shell-command-switch command))
                (error (format "Error: executing command \"%s\" produced error code %d" command return-code)))
              ))))
    (s-trim result)))


(define-advice +python/open-repl (:override ()
                                  +jg-python-env-activate-advice)
  " Auto-detect python repl and activate environment if necessary "
  (require 'python)
  (unless python-shell-interpreter
    (user-error "`python-shell-interpreter' isn't set"))
  ;; look for a venv
  ;; activate environment, start python repl
  (+jg-python-activate-venv-and-conda)

  (let* ((cmd (python-shell-calculate-command))
         (new-buffer (process-buffer
                      (run-python cmd nil t))))
    (puthash (cons 'inferior-python-mode (doom-project-root)) new-buffer +eval-repl-buffers)
    (puthash (cons 'python-mode (doom-project-root)) new-buffer +eval-repl-buffers)
    new-buffer
  )
)

(define-advice python-shell-calculate-command (:override ()
                                               +jg-python-shell-calculate-command)
  "Calculate the string used to execute the inferior Python process.
Adds in a few extra options like dev mode control,
a custom pycache location,
and adding extra pythonpath locations as the pre-args
"
;; `python-shell-make-comint' expects to be able to
;; `split-string-and-unquote' the result of this function.
  (s-join " "
          (--remove (not it)
                    (list
                     (combine-and-quote-strings (list python-shell-interpreter))
                     python-shell-interpreter-args
                     (if jg-python-dev-mode jg-python-dev-cmd)
                     (format jg-python-pycache-cmd (f-canonical jg-python-pycache-loc))
                     "-c \"import sys; print(sys.path)\""
                     )
                    )
          )
  )


(fmakunbound '+python/open-ipython-repl)
(fmakunbound '+python/open-jupyter-repl)

;;; +env.el ends here
