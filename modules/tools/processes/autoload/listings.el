;;; listings.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-processes-list ()
  (interactive)
  (list-processes)
  )

;;;###autoload
(defun +jg-processes-tree ()
  (interactive)
  (with-current-buffer (get-buffer-create "*PSTree*")
    (read-only-mode -1)
    (erase-buffer)
    (call-process "pstree" nil t nil "-p" (format "%s" (emacs-pid)) "-w")
    (read-only-mode 1)
    )
  (display-buffer (get-buffer "*PSTree*"))
  )

;;;###autoload
(defun +jg-processes-tree-all ()
  (interactive)
  (with-current-buffer (get-buffer-create "*PSTree*")
    (read-only-mode -1)
    (erase-buffer)
    (call-process "pstree" nil t nil "-w")
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
     (shell-command "killall evince"))
     )
  )
