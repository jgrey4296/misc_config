;;; lang/python/autoload/conda.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; TODO add registered setup / teardown functions

;;-- vars

;;;###autoload
(defvar env-handling-state nil)

;;;###autoload
(defvar env-handling-registered '((:setup none) (:support none) (:teardown none)) )

;;;###autoload
(defvar env-handling-markers '(".venv" ".conda" "Pipfile" "pyproject.toml"))

;;-- end vars

;;;###autoload
(defun env-handling-go! ()
  "Dispatch to activate appropriate environment
and call the currently used lsp/conda client entrypoint"
  (interactive)
  (message "Handling Python Environment")
  (unless env-handling-state
    (env-handling-state--init))

  ;;-- environment activation
  (let* ((local-env (env-handling-find-venv))
         (env-name (plist-get local-env :env))
         (env-path (plist-get local-env :path))
         (root (or (projectile-project-root) default-directory))
        )
    (unless (plist-get env-handling-state :setup) ;; Select handler
      (setf (plist-get env-handling-state :setup)
            (intern (ivy-read "Python Env Handler: " (env-handling--get-handlers :setup) :require-match t)))
      )
    (unless (plist-get env-handling-state :support)  ;; select support
      (setf (plist-get env-handling-state :support)
            (intern (ivy-read "Python Support: " (env-handling--get-handlers :support) :require-match t)))
      )
    (unless (or env-name (not (eq (plist-get env-handling-state :setup) 'conda)))
      (setq env-name (conda-env-read-name "Select Environment: "))
      )
    (add-to-list 'python-shell-extra-pythonpaths root)
    (add-to-list 'py-shell-extra-pythonpaths root)
    (when (boundp 'lsp-pyright-extra-paths)
      (setq lsp-pyright-extra-paths (vconcat lsp-pyright-extra-paths (vector root))))

    (pcase (plist-get env-handling-state :setup)
      ((and (guard (plist-get env-handling-state :locked)) (guard (not (string-equal (plist-get env-handling-state :env) env-name))))
       (message "Environment is locked"))
      ((and (guard (plist-get env-handling-state :env)) (guard (string-equal (plist-get env-handling-state :env) env-name)))
       (message "Environment is current"))
      ((and 'conda (guard env-name) (guard (not (f-exists? (f-join conda-env-home-directory env-name)))))
       (message "Conda Environment Doesn't exist: %s %s" conda-env-home-directory env-name))
      ((and 'venv (guard env-name) (guard (not (f-exists? (f-join root env-path env-name)))))
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
      ('venv
       (message "Activating via venv")
       (pyvenv-activate (f-join env-path env-name)))
      ('pythonic
       (message "Activating via pythonic")
       (pythonic-activate (f-join env-path env-name)))
      )
    (setf (plist-get env-handling-state :env) env-name
          (plist-get env-handling-state :path) env-path
          )
    )
  ;;-- end environment activation

  ;;-- support activation
  (pcase (plist-get env-handling-state :support)
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

;;;###autoload
(defun env-handling-clear-env! ()
  (interactive)
  (message "Clearing python environment")
  (pcase (plist-get env-handling-state :setup)
    ('pipenv (pipenv-deactivate))
    ('poetry (poetry-venv-deactivate))
    ('conda
     (conda-env-deactivate)
     (setenv "conda_default_env" nil))
    ('venv (pyvenv-deactivate))
    ('pythonic (pythonic-deactivate))
    )
  (pcase (plist-get env-handling-state :support)
    ('eglot (signal 'eglot-todo (current-buffer)))
    ('lsp (lsp-shutdown-workspace lsp--cur-workspace))
    ('tree-sitter (tree-sitter-mode -1))
    ('conda (anaconda-mode-stop) (anaconda-mode -1))
    ('flycheck (flycheck-mode -1))
    )

  (setq env-handling-state nil
        python-shell-extra-pythonpaths nil
        py-shell-extra-pythonpaths nil
        )
  )

;;;###autoload
(defun env-handling-lock! ()
  (interactive)
  (setf (plist-get env-handling-state :locked) (not (plist-get env-handling-state :locked)))
  (message "Env Loc: %s" (plist-get env-handling-state :locked))
  )

;;;###autoload
(defun env-handling-state-line ()
  (if (plist-get env-handling-state :env)
      (format "Python (%s:%s): %s"
              (plist-get env-handling-state :setup)
              (plist-get env-handling-state :support)
              (if (plist-get env-handling-state :locked)
                  (concat "[" (plist-get env-handling-state :env) "]")
                (plist-get env-handling-state :env))
              )
    "")
  )

;;;###autoload
(defun env-handling-report! ()
  (interactive)
  (message "PyEnv: %s" (env-handling-state-line))
)

;;;###autoload
(defun env-handling-auto-kill-support-hook()
  (add-hook 'kill-buffer-hook
            #'env-handling-auto-kill-support-processes-h
            nil 'local)

  )

(defun env-handling-auto-kill-suppport-processes-h ()
  (let ((no-more-pyfiles (and (eq major-mode 'python-mode)
                              (not (delq (current-buffer)
                                         (doom-buffers-in-mode 'python-mode (buffer-list))))))
        )
    (message "Killing Support Processes")
    (pcase (plist-get env-handling-state :support)
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
(defun env-handling-create-env ()
  (interactive)
  (let ((setup (ivy-read "Python Env Handler: " (env-handling--get-handlers :setup) :require-match t))
        (name (read-string "Conda Env to create: "))
        (ver  (format "python=%s" (read-string "Python Version: " "3.11")))
        (packages (split-string (read-string "Packages: ") " " t t))
        )
    (pcase setup
      ("conda" (apply 'call-process "conda" nil nil nil "create" "-n" name ver packages))
      ("venv"  (apply 'call-process "python" nil nil nil "-m" "venv" (read-directory-name "Venv Dir: " default-directory)))
      (""
       )
      )
    )
  )

;;;###autoload
(defun env-handling-add-package ()
  (interactive)
  (let ((setup (ivy-read "Python Env Handler: " (env-handling--get-handlers :setup) :require-match t))
        (packages (split-string (read-string "Packages: ") " " t t))
        )
    (pcase setup
      ("conda" (apply 'call-process "conda" nil nil nil "install" packages))
      ("pip"   (apply 'call-process "pip" nil nil nil "install" packages))
      ("poetry" (poetry-add))
      ("pipenv" (apply 'call-process "pipenv" nil nil nil "install" packages))
      )
    )
  )

(defun env-handling-find-venv (&optional start)
  " Given a starting directory, look in parent dirs
until a environment marker file is found.

return (:path dir-of-venv? :env env-name?)
"
  (let ((root (projectile-project-root))
        (text "")
        markers result)

    (setq markers (--filter (f-exists? (f-join root it)) env-handling-markers))

    (when markers
        (with-temp-buffer
          (insert-file-contents (f-join root (car markers)))
          (goto-char (point-min))
          (cond ((f-ext? (car markers) "toml")
                 (let ((alist (toml:read-from-string (buffer-substring-no-properties (point-min) (line-end-position)))))

                   ))
                ((string-equal (car markers) "Pipfile")
                 (let ((alist (toml:read-from-string (buffer-substring-no-properties (point-min) (line-end-position)))))

                   ))
                (t
                 (setq text
                       (string-trim (buffer-substring-no-properties (point-min) (line-end-position))))
                 )
                )
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

(defun env-handling-state--init()
  (setq env-handling-state (list :support nil
                                 :setup   nil
                                 :env     nil
                                 :path    nil
                                 :locked  nil
                                 :type    nil)
        )
  )

(defun env-handling--get-handlers (type)
  (cl-remove-duplicates (mapcar #'cdr (--filter (eq (car it) type) env-handling-registered)))
  )

(provide 'env-handling)
