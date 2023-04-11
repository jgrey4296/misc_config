;;; spec-hooks.el -*- lexical-binding: t; -*-

;; Template for registering and applying ui settings

(defvar jg-projects-spec-table       (make-hash-table))

;;;###autodef

(defun +jg-projects-add-spec (sym rules &optional priority override)
  "Register a projects spec for evil-projects-lit "
  (when (or (not (gethash sym jg-projects-spec-table)) override)
    (puthash sym rules jg-projects-spec-table)
    )
  )

;;;###autoload
(defun +jg-projects-reapply-specs ()
  " Apply specs to the target "
  (interactive)
  (setq projectile-project-types
        (cl-loop for key being the hash-keys of jg-projects-spec-table
                 using (hash-values val)
                 collect
                 `(,key .
                   ,(apply 'projectile--build-project-plist val)
                   )
                 )
        )
  )
