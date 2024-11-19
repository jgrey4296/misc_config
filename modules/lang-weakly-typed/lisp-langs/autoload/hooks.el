;;; hooks.el -*- lexical-binding: t; -*-
(require 'ffap)
(require 'find-func)

(defvar emacs-lisp-src-dir nil)
(defvar emacs-lisp-c-src-dir nil)

;;;###autoload
(defun +jg-lisp-setup-library-source ()
  (let ((paths-to-add (append
                       (when emacs-lisp-src-dir
                         (ffap-all-subdirs emacs-lisp-src-dir 1))
                       (ffap-all-subdirs (expand-file-name "straight/repos" doom-local-dir) 1)
                       (ffap-all-subdirs (expand-file-name "modules" doom-user-dir))
                       (ffap-all-subdirs (expand-file-name "packages" doom-user-dir))
                       ))
        (result (cl-copy-list find-library-source-path))
        )
    (mapcar #'(lambda (x) (add-to-list 'result x)) paths-to-add)
    result
    )
  )


(autoload 'straight-register-file-modification "straight")

;;;###autoload
(defun +emacs-lisp-init-straight-maybe-h ()
  "Make sure straight sees modifications to installed packages."
  (when (file-in-directory-p (or buffer-file-name default-directory) doom-local-dir)
    (add-hook 'after-save-hook #'straight-register-file-modification
              nil 'local)))
