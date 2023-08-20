;;; advice.el -*- lexical-binding: t; -*-

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
(advice-add 'ibuffer-find-file :override #'+ibuffer--use-counsel-maybe-a)
