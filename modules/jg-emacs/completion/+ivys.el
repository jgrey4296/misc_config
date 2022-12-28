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

(ivy-set-actions '+jg-completion-ivy-workspace
                 '(("r" (lambda (x) (+workspace-rename x (read-string (format "Rename %s -> : " x)))) "Rename")

                   ))

(defun +jg-completion-workspace-switch (x)
  (cond ((string-equal x (+workspace-current-name))
         (+workspace-save x))
        ((+workspace-exists-p x)
         (+workspace-switch x))
        ((-contains? (bookmark-all-names) x)
         (+workspace-switch x t)
         (bookmark-jump x))
        (t (+workspace-switch x t))
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

(defun +jg-completion-yas-prompt-fn (prompt choices &optional display-fn)
  " Yasnippet ivy which shows groups "
  (let* ((max-name 0)
         (max-group 0)
         (index 0)
         ;; Get the bits i care about to create the display
         (choice-parts (cl-loop for template in choices
                                collect
                                (let ((name (s-replace "\\" "/" (s-trim (yas--template-name template))))
                                      (group (s-trim (apply 'concat (yas--template-group template))))
                                      (table (concat "Table:" (yas--table-name (yas--template-table template))))
                                      )
                                  (cl-incf index)
                                  (setq max-name (max (length name) max-name)
                                        max-group (max (length group) max-group))
                                  (list (list name (length name)) (list group (length group)) table (1- index)))))
         (format-fn (lambda (x) `(,(concat " "
                                           ;; template name + gap
                                           (caar x) (make-string (- (+ 5 max-name) (cadar x)) ? ) " : "
                                           ;; groups
                                           (caadr x) (make-string (- (+ 5 max-group) (cadadr x)) ? ) " : "
                                           ;; table
                                           (caddr x))
                                  ;; index
                                  . ,(car (last x)))))
         chosen)
    ;; only once i know the max-name, format the choices
    ;; use :action rather than return value, to use the (str . index) pair
    (ivy-read "Choose Snippet: " (mapcar format-fn choice-parts) :caller '+jg-completion-yas-ivy :action (lambda (x) (setq chosen (cdr x))))
    (nth (or chosen 0) choices)
    )
  )

(defun +jg-completion-ivy-bookmark ()
  "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist.
Modified to pre-sort bookmarks, caselessly
"
  (interactive)
  (require 'bookmark)
  (ivy-read "Create or jump to bookmark: "
            (cl-sort (bookmark-all-names) #'string-lessp :key #'downcase)
            :history 'bookmark-history
            :action (lambda (x)
                      (cond ((and counsel-bookmark-avoid-dired
                                  (member x (bookmark-all-names))
                                  (file-directory-p (bookmark-location x)))
                             (with-ivy-window
                               (let ((default-directory (bookmark-location x)))
                                 (counsel-find-file))))
                            ((member x (bookmark-all-names))
                             (with-ivy-window
                               (bookmark-jump x)))
                            (t
                             (bookmark-set x))))
            :caller 'counsel-bookmark))

(defun +jg-completion-ivy-features ()
  " Insert from a list of recognized features "
  (interactive)
  (ivy-read "Available Features: "
            (seq-sort #'string-lessp features)
            :require-match nil
            :action 'insert
            )
  )

(defun +jg-completion-ivy-workspace ()
    "Switch to a workspace or create a new one"
    (interactive)
    (require 'bookmark)
    (ivy-read "Create or jump to workspace: "
              (+workspace-list-names)
              :history 'workspace-history
              :action '+jg-completion-workspace-switch
              :caller '+jg-completion-ivy-workspace)
    )
;;-- end ivys

;;-- compilation action advice
(define-advice counsel-compile--action (:override (cmd)
                                                  +jg-counsel-compile-adjust)
  "call cmd, but if it's got the text propery 'cmd use that instead "
  (let ((blddir (get-text-property 0 'blddir cmd))
        (bldenv (get-text-property 0 'bldenv cmd)))
    (if (get-text-property 0 'recursive cmd)
        (counsel-compile blddir)
      (when (get-char-property 0 'cmd cmd)
        (setq cmd (get-text-property 0 'cmd cmd)))
      (let ((default-directory (or blddir
                                   counsel-compile--current-build-dir
                                   default-directory))
            (compilation-environment bldenv))
        ;; No need to specify `:history' because of this hook.
        (add-hook 'compilation-start-hook #'counsel-compile--update-history)
        (unwind-protect
             (compile cmd)
          (remove-hook 'compilation-start-hook #'counsel-compile--update-history))))))


;;-- end compilation action advice

;;-- doit command retrieval
(defun +jg-completion-get-doit-commands (&optional dir)
  (interactive)
  ;; counsel-compile-local-builds
  (let ((default-directory (or dir (projectile-project-root) default-directory))
        result-code
        result-text
        )
    (with-temp-buffer
      (setq result-code (call-process "doit" nil (current-buffer) nil "list")
            result-text (buffer-string)
            )
      )
    (if (eq 0 result-code)
        (cl-loop for line in (split-string result-text "\n" t " \n")
                 collect (let ((thestr line)
                               (cmdstr (concat "doit " (car (split-string line " " t " "))))
                               )
                           (set-text-properties 0 (length thestr) `(cmd ,cmdstr) thestr)
                           thestr)

             )
      '()
      )
    )
  )


;;-- end doit command retrieval

;;-- gradle
(defun +jg-completion-get-gradle-commands (&optional dir)
  (interactive)
  (let ((default-directory (or dir (projectile-project-root) default-directory))
        result-code
        result-text
        )
    (with-temp-buffer
      (setq result-code (call-process "gradle" nil (current-buffer) nil "tasks"))
      (goto-char (point-min))
      (keep-lines "^\\w+ -")
      (align-regexp (point-min) (point-max) "\\(\\s-*\\)-")
      (setq result-text (buffer-string))
      )
    (if (eq 0 result-code)
        (cl-loop for line in (split-string result-text "\n" t " \n")
                 collect (let ((thestr line)
                               (cmdstr (concat "gradle " (car (split-string line "-" t " "))))
                               )
                           (set-text-properties 0 (length thestr) `(cmd ,cmdstr) thestr)
                           thestr)
             )
      '()
      )
    )
  )
;;-- end gradle
