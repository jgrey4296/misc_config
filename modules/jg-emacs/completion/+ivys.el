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

(defun +jg-completion-ivy-kill-buffer (buff)
  (interactive)
  (with-current-buffer buff
    (kill-current-buffer)
    )
  )

;;-- end utils

;;-- actions
;; Overrides find-file's actions to add "f" for find literally
(ivy-add-actions 'counsel-find-file
                 '(("f" (lambda (x) (find-file-literally x)) "Fundamental")))

(ivy-set-actions 'ivy-switch-buffer
                 '(("p" +jg-completion-ivy-open-as-popup "Popup")
                   ("k" +jg-completion-ivy-kill-buffer "Kill")
                   ))

(ivy-set-actions '+jg-completion-counsel-workspace
                 '(("r" (lambda (x) (+workspace-rename x (read-string (format "Rename %s -> : " x)))))

                   )
)
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
            :predicate       #'+jg-completion-ivy-predicate
            :action          #'+jg-completion-ivy-open-as-popup
            :matcher         #'ivy--switch-buffer-matcher
            :caller 'window-control-ivy-popup-buffer
            )
  )

(defun +jg-yas-prompt-fn (prompt choices &optional display-fn)
  " Yasnippet ivy which shows groups "
  (let* ((max-name 0)
         (choice-parts (cl-loop for template in choices
                                collect
                                (let ((name (s-replace "\\" "/" (s-trim (yas--template-name template))))
                                      (group (s-trim (apply 'concat (yas--template-group template))))
                                      )
                                  (setq max-name (max (length name) max-name))
                                  (list name group (length name)))))
         (format-fn (lambda (x) (concat " " (car x) (make-string (- (+ 5 max-name) (nth 2 x)) ? ) " : " (cadr x))))
         chosen)
    (setq chosen (ivy-read "Choose Snippet: " (mapcar format-fn choice-parts) :caller '+jg-completion-yas-ivy))
    (nth (or (cl-position chosen formatted-choices :test #'string=) 0) choices)
    )
  )

;;-- end ivys
