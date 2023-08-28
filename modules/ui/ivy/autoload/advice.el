;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ivy--run-from-ivy-directory-a (fn &rest args)
  " HACK Fix an issue where `counsel-projectile-find-file-action' would try to
        open a candidate in an occur buffer relative to the wrong buffer,
        causing it to fail to find the file we want. "
  (let ((default-directory (ivy-state-directory ivy-last)))
    (apply fn args)))

;;;###autoload
(defun  +ivy--counsel-file-jump-use-fd-rg-a (args)
  "Change `counsel-file-jump' to use fd or ripgrep, if they are available."

  (cl-destructuring-bind (find-program . args)
      (cond ((when-let (fd (executable-find (or doom-projectile-fd-binary "fd") t))
               (append (list fd "--hidden" "--type" "file" "--type" "symlink" "--follow" "--color=never")
                       (cl-loop for dir in projectile-globally-ignored-directories
                                collect "--exclude"
                                collect dir)
                       (if IS-WINDOWS '("--path-separator=/")))))
            ((executable-find "rg" t)
             (append (list "rg" "--hidden" "--files" "--follow" "--color=never" "--no-messages")
                     (cl-loop for dir in projectile-globally-ignored-directories
                              collect "--glob"
                              collect (concat "!" dir))
                     (if IS-WINDOWS '("--path-separator=/"))))
            ((cons find-program args)))
    (unless (listp args)
      (user-error "`counsel-file-jump-args' is a list now, please customize accordingly."))
    (counsel--call
     (cons find-program args)
     (lambda ()
       (goto-char (point-min))
       (let (files)
         (while (< (point) (point-max))
           (push (buffer-substring (line-beginning-position) (line-end-position))
                 files)
           (forward-line 1))
         (nreverse files))))))

;;;###autoload
(advice-add 'counsel-projectile-find-file-action :around #'+ivy--run-from-ivy-directory-a)

;;;###autoload
(advice-add 'counsel--find-return-list :override #'+ivy--counsel-file-jump-use-fd-rg-a)
