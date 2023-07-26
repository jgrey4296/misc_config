;;; hooks.el -*- lexical-binding: t; -*-
(require 'ffap)
(require 'find-func)


;;;###autoload
(defun +jg-lisp-setup-library-source ()
  (let ((paths-to-add (append
                       (ffap-all-subdirs "/Volumes/documents/github/_libs/lisp/emacs-src/lisp/" 1)
                       (ffap-all-subdirs (expand-file-name "straight/repos" doom-local-dir) 1)
                       (ffap-all-subdirs (expand-file-name "modules" doom-user-dir))
                       (ffap-all-subdirs (expand-file-name "packages" doom-user-dir))
                       )))
  (mapc (lambda (x)
          (add-to-list 'find-library-source-path x))
        paths-to-add)
  )
  (setq find-function-C-source-directory "/Volumes/documents/github/_libs/lisp/emacs-src/src")

  )


(autoload 'straight-register-file-modification "straight")

;;;###autoload
(defun +emacs-lisp-init-straight-maybe-h ()
  "Make sure straight sees modifications to installed packages."
  (when (file-in-directory-p (or buffer-file-name default-directory) doom-local-dir)
    (add-hook 'after-save-hook #'straight-register-file-modification
              nil 'local)))
