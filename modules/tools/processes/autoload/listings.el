;;; listings.el -*- lexical-binding: t; -*-

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

(defun jg--process-buffers-p (keyval)
  (get-buffer-process (cdr keyval))
  )

;;;###autoload
(defun +jg-processes-buffer-ivy ()
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :predicate           #'jg--process-buffers-p
            :action              #'ivy--switch-buffer-action
            :matcher             #'ivy--switch-buffer-matcher
            :sort t
            :caller 'ivy-switch-buffer)
  )
