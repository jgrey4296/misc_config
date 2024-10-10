;;; listings.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-processes-list ()
  (interactive)
  (list-processes)
  )

;;;###autoload
(defun +jg-processes-tree ()
  "get the process tree for this emac"
  (interactive)
  (with-current-buffer (get-buffer-create "*PSTree*")
    (read-only-mode -1)
    (erase-buffer)
    (call-process "pstree" nil t nil "-p" (format "%s" (emacs-pid)))
    (read-only-mode 1)
    )
  (display-buffer (get-buffer "*PSTree*"))
  )

;;;###autoload
(defun +jg-processes-tree-all ()
  "get the entire system process tree"
  (interactive)
  (with-current-buffer (get-buffer-create "*PSTree*")
    (read-only-mode -1)
    (erase-buffer)
    (call-process "pstree" nil t nil)
    (read-only-mode 1)
    )
  (display-buffer (get-buffer "*PSTree*"))
  )


;;;###autoload
(defun +jg-processes-kill-preview ()
  (interactive)
  (pcase system-type
    ('darwin
     (shell-command "killall Preview"))
    ('gnu/linux
     (shell-command "killall -q -I evince")
     )
    )
  )
