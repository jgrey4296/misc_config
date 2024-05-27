;;; advice.el -*- lexical-binding: t; -*-

(defvar +jg-ibuffer-marked-list nil)

;;;###autoload
(defun +ibuffer--use-counsel-maybe-a (_file &optional _wildcards)
  (interactive
   (let* ((buf (ibuffer-current-buffer))
          (default-directory (if (buffer-live-p buf)
                                 (with-current-buffer buf
                                   default-directory)
                               default-directory)))
     (list (counsel--find-file-1 "Find file: " nil
                                 #'identity
                                 'counsel-find-file) t)))
  (find-file _file _wildcards)
  )

;;;###autoload
(defun  +ibuffer-populate-marked-list-for-sorting (&rest args)
  (with-current-buffer "*Ibuffer*"
    (setq +jg-ibuffer-marked-list (ibuffer-get-marked-buffers))
    )
  )
