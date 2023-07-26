;;; ivy.el -*- lexical-binding: t; -*-

(defvar jg-help-modules-cache nil)

(defvar jg-help-binding-file-name "+bindings.el")

(defvar jg-help-config-file-name "config.el")

(defvar jg-help-var-file-name "+vars.el")

(defvar jg-help-specdef-file-name "+spec-defs.el")

(defun +jg-help-cache-modules ()
  (let* ((root doom-user-dir)
         (groups (f-directories (f-join doom-user-dir "modules")))
         (modules (cl-loop for group in groups
                           append (mapcar #'(lambda (x) (f-join (f-base group) (f-base x)))
                                          (f-directories group))))
         )
    (setq jg-help-modules-cache modules)
    )
  )

;;;###autoload
(defun +jg-help-edit-bindings ()
  (interactive)
  (unless jg-help-modules-cache (+jg-help-cache-modules))
  (let* ((bindings (-select #'(lambda (x) (f-exists? (f-join doom-user-dir "modules" x jg-help-binding-file-name))) jg-help-modules-cache))
         (chosen (ivy-read "Select Module Bindings: " jg-help-modules-cache :require-match t))
         (binding (f-join doom-user-dir "modules" chosen jg-help-binding-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )

;;;###autoload
(defun +jg-help-edit-vars ()
  (interactive)
  (unless jg-help-modules-cache (+jg-help-cache-modules))
  (let* ((vars (-select #'(lambda (x) (f-exists? (f-join doom-user-dir "modules" x jg-help-var-file-name))) jg-help-modules-cache))
         (chosen (ivy-read "Select Module Vars: "  jg-help-modules-cache :require-match t))
         (varf (f-join doom-user-dir "modules" chosen jg-help-var-file-name))
        )
    (if (f-exists? varf)
      (find-file varf)
      (message "Doesnt Exist: %s" varf)
      )
    )
  )

;;;###autoload
(defun +jg-help-edit-config ()
  (interactive)
  (unless jg-help-modules-cache (+jg-help-cache-modules))
  (let* ((config (-select #'(lambda (x) (f-exists? (f-join doom-user-dir "modules" x jg-help-config-file-name))) jg-help-modules-cache))
         (chosen (ivy-read "Select Module Config: " jg-help-modules-cache :require-match t))
         (binding (f-join doom-user-dir "modules" chosen jg-help-config-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )


;;;###autoload
(defun +jg-help-edit-spec-defs ()
  (interactive)
  (unless jg-help-modules-cache (+jg-help-cache-modules))
  (let* ((config (-select #'(lambda (x) (f-exists? (f-join doom-user-dir "modules" x jg-help-config-file-name))) jg-help-modules-cache))
         (chosen (ivy-read "Select Module Config: " jg-help-modules-cache :require-match t))
         (binding (f-join doom-user-dir "modules" chosen jg-help-specdef-file-name))
        )
    (if (f-exists? binding)
      (find-file binding)
      (message "Doesnt Exist: %s" binding)
      )
    )
  )
