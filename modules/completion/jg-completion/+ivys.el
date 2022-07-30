;;; +ivys.el -*- lexical-binding: t; -*-

;;-- utils
(defun +jg-completion-ivy-predicate (x)
  ;; return nil for cruft buffers
  (not (string-match jg-completion-ivy-predicate-patterns (car x)))
  )

(defun +jg-completion-ivy-open-as-popup (buff)
  (interactive)
  (let ((curr-rule (display-buffer-assq-regexp buff display-buffer-alist nil))
        (curr-window (selected-window))
        )
    ;; Add rule if necessary:
    (if (not curr-rule)
        (progn (message "Adding temp rule")
               (setq curr-rule (+popup-make-rule buff window-control-popup-persist-default))
               (push curr-rule display-buffer-alist))
      )
    (bury-buffer buff)
    (pop-to-buffer buff)
    (if (not (alist-get 'select (alist-get 'window-parameters curr-rule)))
        (select-window curr-window)
      )
    )
  )

;;-- end utils


;;-- actions
;; Overrides find-file's actions to add "f" for find literally
(ivy-add-actions 'counsel-find-file
                 '(("f" (lambda (x) (find-file-literally x)) "Fundamental")))

(ivy-set-actions 'ivy-switch-buffer
                 '(("p" +jg-completion-ivy-open-as-popup "Popup")))

;;-- end actions


;;-- ivys
(defun +jg-completion-switch-buffer ()
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :predicate #'+jg-completion-ivy-predicate
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :sort t
            :caller 'ivy-switch-buffer)
  )

(defun +jg-completion-popup-buffer ()
  (interactive)
  (ivy-read "Popup Buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :predicate       #'+jg-ui-ivy-predicate
            :action          #'+jg-completion-ivy-open-as-popup
            :matcher         #'ivy--switch-buffer-matcher
            :caller 'window-control-ivy-popup-buffer
            )
  )
;;-- end ivys







;;-- advice
(define-advice projectile-run-compilation (:filter-args (val)
                                           +jg-completion-command-expander)
  " Expand variables mentioned in the command "
  (let ((loc (if (eq major-mode 'dired-mode)
                 (dired-current-directory)
               (f-parent (buffer-file-name)))))
    (list (s-replace "\$" (format "TEST_TARGET=\"%s\"" loc) (car val)))
    )
  )
(define-advice projectile--run-project-cmd (:around (fn &rest rst)
                                            +jg-completion-projectile-cmd-list)
  " Use an ivy to get the command "
  (let* ((compilation-read-command nil)
        (root (projectile-project-root))
        (cmd-cache (f-join root jg-misc-project-cmd-cache-name))
        (candidates (if (and root (f-exists? cmd-cache))
                        (with-temp-buffer
                          (insert-file-contents cmd-cache)
                          (sort (s-split "\n" (buffer-string) t) 'string-lessp))))
        (ivy-val (ivy-read (plist-get rst :prompt-prefix)
                           candidates)))
    (if (not (-contains? candidates ivy-val))
        (append-to-file (format "%s\n" ivy-val) nil cmd-cache)
        )
    (apply fn (cons ivy-val (cdr rst)))
    )
  )
;;-- end advice
