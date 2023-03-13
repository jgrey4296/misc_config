;;; emacs/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +dired-enable-git-info-h ()
  "Enable `dired-git-info-mode' in git repos."
  (and (not (file-remote-p default-directory))
       (locate-dominating-file "." ".git")
       (dired-git-info-mode 1)))
