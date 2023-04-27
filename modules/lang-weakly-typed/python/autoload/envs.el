;;; lang/python/autoload/conda.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;-- vars

;;;###autoload
(defvar jg-python-env-state nil)

(defvar jg-conda-activate-cmd "source $HOME/.doom.d/terminal/bash/conda.bash && activate %s")

;;;###autoload
(defvar jg-python-env-registered '((:activator none) (:support none)) )

;;-- end vars

(defun +jg-python-env-state--init()
  (setq jg-python-env-state (list :support nil
                                  :activator nil
                                  :env nil
                                  :path nil
                                  :locked nil)
        )
  )

;;;###autoload
(defun +jg-python-env-state-line ()
  (if (plist-get jg-python-env-state :env)
      (format "Python (%s:%s): %s"
              (plist-get jg-python-env-state :activator)
              (plist-get jg-python-env-state :support)
              (if (plist-get jg-python-env-state :locked)
                  (concat "[" (plist-get jg-python-env-state :env) "]")
                (plist-get jg-python-env-state :env))
              )
    "")
  )

;;;###autoload
(defun +jg-python-env-report! ()
  (interactive)
  (message "PyEnv: %s" (+jg-python-env-state-line))
)

;;;###autoload
(defun +jg-python-clear-env! ()
  (interactive)
  (message "Clearing python environment")
  (pcase (plist-get jg-python-env-state :activator)
    ('pipenv (pipenv-deactivate))
    ('poetry (poetry-venv-deactivate))
    ('conda
     (conda-env-deactivate)
     (setenv "conda_default_env" nil))
    ('pyvenv (pyvenv-deactivate))
    ('pythonic (pythonic-deactivate))
    )
  (pcase (plist-get jg-python-env-state :support)
    ('eglot (signal 'eglot-todo (current-buffer)))
    ('lsp (lsp-shutdown-workspace lsp--cur-workspace))
    ('tree-sitter (tree-sitter-mode -1))
    ('conda (anaconda-mode-stop) (anaconda-mode -1))
    ('flycheck (flycheck-mode -1))
    )

  (setq jg-python-env-state nil
        python-shell-extra-pythonpaths nil
        py-shell-extra-pythonpaths nil
        )
  )

(defun +jg-python-env--get-handlers (type)
  (cl-remove-duplicates (mapcar #'cdr (--filter (eq (car it) type) jg-python-env-registered)))
  )

;;;###autoload
(defun +jg-python-env-lock! ()
  (interactive)
  (setf (plist-get jg-python-env-state :locked) (not (plist-get jg-python-env-state :locked)))
  (message "Env Loc: %s" (plist-get jg-python-env-state :locked))
  )

;;;###autoload
(defun +jg-python-handle-env! ()
  "Dispatch to activate appropriate environment
and call the currently used lsp/conda client entrypoint"
  (interactive)
  (message "Handling Python Environment")
  (unless jg-python-env-state
    (+jg-python-env-state--init))

  ;;-- environment activation
  (let* ((local-env (+jg-python-find-venv))
         (env-name (plist-get local-env :env))
         (env-path (plist-get local-env :path))
         (root (or (projectile-project-root) default-directory))
        )
    (unless (plist-get jg-python-env-state :activator) ;; Select handler
      (setf (plist-get jg-python-env-state :activator)
            (intern (ivy-read "Python Env Handler: " (+jg-python-env--get-handlers :activator) :require-match t)))
      )
    (unless (plist-get jg-python-env-state :support)  ;; select support
      (setf (plist-get jg-python-env-state :support)
            (intern (ivy-read "Python Support: " (+jg-python-env--get-handlers :support) :require-match t)))
      )
    (unless (or env-name (not (eq (plist-get jg-python-env-state :activator) 'conda)))
      (setq env-name (conda-env-read-name "Select Environment: "))
      )
    (add-to-list 'python-shell-extra-pythonpaths root)
    (add-to-list 'py-shell-extra-pythonpaths root)
    (when (boundp 'lsp-pyright-extra-paths)
      (setq lsp-pyright-extra-paths (vconcat lsp-pyright-extra-paths (vector root))))

    (pcase (plist-get jg-python-env-state :activator)
      ((and (guard (plist-get jg-python-env-state :locked)) (guard (not (string-equal (plist-get jg-python-env-state :env) env-name))))
       (message "Environment is locked"))
      ((and (guard (plist-get jg-python-env-state :env)) (guard (string-equal (plist-get jg-python-env-state :env) env-name)))
       (message "Environment is current"))
      ((and 'conda (guard env-name) (guard (not (f-exists? (f-join conda-env-home-directory env-name)))))
       (message "Conda Environment Doesn't exist: %s %s" conda-env-home-directory env-name))
      ((and 'pyvenv (guard env-name) (guard (not (f-exists? (f-join root env-path env-name)))))
       (message "Virtual environment doesn't exist: %s : %s : %s" root env-path env-name))
      ((and 'pipenv (guard (not (pipenv-project-p))))
       (message "Not a pipenv project"))
      ((and 'poetry (guard (not (poetry-ensure-in-project))))
       (message "Not in a poetry project"))
      ('pipenv
       (message "Activating pipenv")
       (pipenv-activate))
      ('poetry
       (message "Activating via poetry")
       (poetry-venv-workon))
      ((and 'conda (guard env-name) (guard (not (string-empty-p env-name))))
       (message "Activating Conda Environment: %s" env-name)
       (conda-env-activate env-name)
       (setenv "CONDA_DEFAULT_ENV" env-name))
      ('pyvenv
       (message "Activating via pyvenv")
       (pyvenv-activate (f-join env-path env-name)))
      ('pythonic
       (message "Activating via pythonic")
       (pythonic-activate (f-join env-path env-name)))
      )
    (setf (plist-get jg-python-env-state :env) env-name
          (plist-get jg-python-env-state :path) env-path
          )
    )
  ;;-- end environment activation

  ;;-- support activation
  (pcase (plist-get jg-python-env-state :support)
    ('eglot
     (message "Adding eglot support")
     (eglot-ensure))
    ('lsp
     (message "Adding lsp support")
     (lsp-deferred))
    ('tree-sitter
     (message "Adding tree-sitter support")
     (tree-sitter!))
    ('conda
     (message "Adding conda support")
     (anaconda-mode 1))
    ('flycheck
     (message "Adding flycheck support")
     (unless flycheck-enabled-checkers
       (let ((chosen (intern (ivy-read "Flychecker: " flycheck-disabled-checkers :require-match t))))
         (delete chosen flycheck-disabled-checkers)
         (add-to-list flycheck-enabled-checkers chosen)
         ))
     (flycheck-mode 1))
    )
  ;;-- end support activation

  )

(defun +jg-python-find-venv (&optional start)
  " Given a starting directory, look in parent dirs
until a .venv file is found.

return (:path dir-of-venv? :env env-name?)
"
  (let ((root (projectile-project-root))
        (text "")
        result)

      (when (and root (f-exists? (f-join root ".venv")))
        (with-temp-buffer
          (insert-file-contents (f-join root".venv"))
          (goto-char (point-min))
          (setq text
                (string-trim (buffer-substring-no-properties (point-min) (line-end-position))))
          )
        )

      (list :path (pcase (f-parent text)
                    ((pred string-empty-p) nil)
                    ("/" root)
                    ("./" root)
                    ("../" root)
                    (v v)
                    )
            :env (pcase (f-filename text)
                   ((pred string-empty-p) nil)
                   (v v)
                   )
            )
      )
  )

;;;###autoload
(defun +jg-python-auto-kill-support-hook()
  (add-hook 'kill-buffer-hook
            #'+jg-python-auto-kill-support-processes-h
            nil 'local)

  )

(defun +jg-python-auto-kill-suppport-processes-h ()
  (let ((no-more-pyfiles (and (eq major-mode 'python-mode)
                              (not (delq (current-buffer)
                                         (doom-buffers-in-mode 'python-mode (buffer-list))))))
        )
    (message "Killing Support Processes")
    (pcase (plist-get jg-python-env-state :support)
      ((and 'lsp (guard no-more-pyfiles))
       (lsp-disconnect)
       (lsp-workspace-shutdown lsp--cur-workspace))
      ('lsp
       (lsp-disconnect))
      ((and 'conda (guard no-more-pyfiles))
       (anaconda-mode-stop)
       (anaconda-eldoc-mode -1))
      ('flycheck
       (flycheck-mode -1)
       )
      )
    )
  )

;;;###autoload
(defun +python/set-conda-home ()
  "Set `conda-anaconda-home' (ANACONDA_HOME).

Usually it's `~/.anaconda3' on local machine, but it can be set to a remote
directory using TRAMP syntax, e.g. `/ssh:host:/usr/bin/anaconda3'. This way, you
can use a remote conda environment, including the corresponding remote python
executable and packages."
  (interactive)
  (require 'conda)
  (when-let (home (read-directory-name "Set conda home: " "~" nil nil conda-anaconda-home))
    (setq conda-anaconda-home home)
    (message "Successfully changed conda home to: %s" (abbreviate-file-name home))))

;;;###autoload
(defun +jg-python-create-venv ()
  (interactive)
  (let ((dir (f-relative (read-directory-name "Venv dir: " (projectile-project-root))
                         (projectile-project-root)))
        )
    (call-process "python" nil nil nil "-m" "venv" dir)
    (with-temp-buffer
      (insert dir)
      (write-file (f-join (projectile-project-root) ".venv"))
      )
    )
)
