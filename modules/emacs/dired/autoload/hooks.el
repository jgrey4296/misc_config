
;;;###autoload
(defun +dired-disable-gnu-ls-flags-maybe-h ()
  "Remove extraneous switches from `dired-actual-switches' when it's
uncertain that they are supported (e.g. over TRAMP or on Windows).

Fixes #1703: dired over TRAMP displays a blank screen.
Fixes #3939: unsortable dired entries on Windows."
  (when (or (file-remote-p default-directory)
            (and (boundp 'ls-lisp-use-insert-directory-program)
                 (not ls-lisp-use-insert-directory-program)))
    (setq-local dired-actual-switches (car args))))
