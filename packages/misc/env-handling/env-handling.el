;;; lang/python/autoload/conda.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; TODO add registered setup / teardown functions

;;-- vars

;;;###autoload
(defvar env-handling-state nil)

;;;###autoload
(defvar env-handling-registered '((:setup none)
                                  (:support none)
                                  (:teardown none)
                                  (:create none)
                                  (:install none)
                                  (:update none)
                                  )
  )

;;;###autoload
(defvar env-handling-markers '(".venv" ".conda" "Pipfile" "pyproject.toml"))

(defconst env-handling-process-name "env-handling-proc")
(defconst env-handling-buffer-name "*env-handling*")

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
         (env-name (plist-get local-env :name))
         (env-path (plist-get local-env :path))
         (root (or (projectile-project-root) default-directory))
        )
    (unless (plist-get env-handling-state :setup) ;; Select handler
      (setf (plist-get env-handling-state :setup)
            (let* ((vals (env-handling--get-handlers :setup))
                   (chosen (ivy-read "Python Env Handler: " vals :require-match t))
                  )
              (cons (intern chosen) (alist-get (intern chosen) vals))))
      )
    (unless (plist-get env-handling-state :support)  ;; select support
      (setf (plist-get env-handling-state :support)
            (let* ((vals (env-handling--get-handlers :support))
                   (chosen (ivy-read "Python Support: " vals :require-match t)))
              (cons (intern chosen) (alist-get (intern chosen) vals))))
      )
    (add-to-list 'python-shell-extra-pythonpaths root)
    (add-to-list 'py-shell-extra-pythonpaths root)

    (let ((setup (plist-get env-handling-state :setup))
          (locked (plist-get env-handling-state :locked))
          (curr-env (plist-get env-handling-state :env))
          final-env
          )
      (pcase (car-safe setup)
      ;;-- failures
      ((and (guard locked) (guard (not (string-equal curr-env env-name))))
       (message "Environment is locked"))
      ((and (guard curr-env) (guard (string-equal curr-env env-name)))
       (message "Environment is current"))
      ((and (guard env-path) (guard env-name) (guard (not (f-exists? (f-join env-path env-name)))))
       (message "Environment Path Doesn't exist: %s %s" env-path env-name))
       ;;-- end failures
      ((and (let setup-fn (cadr setup)) (guard (functionp setup-fn)))
       (message "Activating %s" (car setup))
       (setq final-env (funcall setup-fn env-handling-state local-env))
       )
      )
    (setf (plist-get env-handling-state :env) (plist-get (or final-env local-env) :name)
          (plist-get env-handling-state :path) (plist-get (or final-env local-env) :path)
          )
    )
    ;;-- end environment activation

    ;;-- support activation
    (let ((support (plist-get env-handling-state :support)))
      (pcase (car-safe support)
        ((and (let support-fn (cadr support)) (guard (functionp support-fn)))
         (message "Activating %s" (car support))
         (funcall support-fn env-handling-state))
        ('none nil)
        (_ (message "Support Lacks a setup function"))
        )
      )
    ;;-- end support activation
    )
  )

;;;###autoload
(defun env-handling-clear-env! ()
  (interactive)
  (let ((setup (plist-get env-handling-state :setup)))
    (message "Clearing python environment")
    (pcase (car-safe setup)
      ((and (let teardown-fn (caddr setup)) (guard (functionp teardown-fn)))
       (funcall teardown-fn env-handling-state))
      ('none nil)
      (_
       (message "No Env Teardown function found: %s" setup))
      )
    )

  (let ((support (plist-get env-handling-state :support)))
    (message "Clearing python support")
    (pcase (car-safe support)
       ((and (let teardown-fn (caddr support)) (guard (functionp teardown-fn)))
        (funcall teardown-fn env-handling-state))
       ('none nil)
       (_
        (message "No Support Teardown function found: %s" support))
      )
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
              (car-safe (plist-get env-handling-state :setup))
              (car-safe (plist-get env-handling-state :support))
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
  (let* ((no-more-pyfiles (and (eq major-mode 'python-mode)
                               (not (delq (current-buffer)
                                          (doom-buffers-in-mode 'python-mode (buffer-list))))))

         (curr     (car-safe (plist-get env-handling-state :support)))
         (teardown (cadr (alist-get curr (env-handling--get-handlers :teardown))))
         )
    (message "Killing Support Processes")
    (pcase teardown
      ((pred functionp)
       (funcall teardown env-handling-state))
      (_ nil)
      )
    )
  )

;;;###autoload
(defun env-handling-create-env! ()
  (interactive)
  (let* ((handlers (env-handling--get-handlers :create))
         (type (ivy-read "Python Env Handler: " handlers :require-match t))
         (handler (car (alist-get (intern type) handlers)))
        )
    (pcase handler
      ((pred functionp)
       (funcall handler env-handling-state))
      (x (message "Unrecognized env setup choice: %s" handler))
      )
    )
  )

;;;###autoload
(defun env-handling-add-package! ()
  (interactive)
  (let* ((handlers (env-handling--get-handlers :install))
         (chosen (ivy-read "Python Env Handler: " handlers :require-match t))
         (handler (cadr (alist-get (intern chosen) handlers)))
        )
    (pcase handler
      ((pred functionp)
       (funcall handler env-handling-state))
      (_ (message "No handler found"))
      )
    )
  )

;;;###autoload
(defun env-handling-update! ()
  (interactive)
  (let* ((handlers (env-handling--get-handlers :update))
         (chosen (ivy-read "Python Env Handler: " handlers :require-match t))
         (handler (alist-get (intern chosen) handlers))
        )
    (pcase handler
      ((pred functionp)
       (funcall handler env-handling-state))
      (_ (message "No handler found"))
      )
    )
  )

(defun env-handling-find-venv (&optional start)
  " Given a starting directory, look in parent dirs
until a environment marker file is found.

return (:marker path-of-dotvenv? :env env-name? :path dir-of-venv? )
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
                   (message "Don't know how to handle toml files yet")
                   ))
                ((string-equal (f-filename (car markers)) "Pipfile")
                 (let ((alist (toml:read-from-string (buffer-substring-no-properties (point-min) (line-end-position)))))
                   (message "Don't know how to handle pipfiles yet")
                   ))
                ((string-equal (f-filename (car markers)) ".conda")
                 (message "need to do .conda"))
                (t
                 (setq text
                       (string-trim (buffer-substring-no-properties (point-min) (line-end-position))))
                 )
                )
          )
      )

    (let* ((parts (split-string text ":" t " +"))
           (head (car-safe parts))
           )

      (list :marker (f-join root (car markers))
            :name (pcase head
                   ('nil nil)
                   ("conda"
                    (cadr parts))
                   ((pred f-directory?)
                    (f-filename head))
                   (_ head)
                   )
            :path (pcase head
                    ('nil nil)
                    ("conda" conda-env-home-directory)
                    ((pred f-directory?)
                     (f-base head))
                    ((guard (f-directory? (f-join root ".temp/venv")))
                     (f-join root ".temp/venv"))
                    (_ conda-env-home-directory)
                    )
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
