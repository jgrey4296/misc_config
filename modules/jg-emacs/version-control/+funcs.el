;;; +funcs.el -*- lexical-binding: t; -*-

(defvar +jg-vcs-task-hash (make-hash-table :test 'equal))
(defvar +jg-vcs-gradle-command "gradlew")
(defvar +jg-vcs-gradle-command-args '())

(after! ivy
  (ivy-configure 'jg-vcs-gradle-ivy
    :format-fn #'jg-vcs-format-gradle)
)

(defun +jg-vcs-format-gradle (cands)
  " Custom ivy format function to align and use the hashtable values"
  (let* ((maxlen (+ 5 (apply 'max (mapcar 'length (hash-table-keys +jg-vcs-task-hash)))))
         (base-func (lambda (cand)
                      (concat (substring-no-properties cand)
                              (make-string (- maxlen (length cand)) ? )
                              "-"
                              (gethash (substring-no-properties cand) +jg-vcs-task-hash)))
                    )
         ;; (sel-func (lambda (x) (concat "> " (funcall base-func x))))
         (sel-func (lambda (x) (propertize (funcall base-func x) 'face 'ivy-highlight-face)))
         )
    (ivy--format-function-generic
     sel-func
     base-func
     cands
     "\n"
     )
    )
  )

(defun +jg-vcs-run-gradle (prefix)
  (interactive "P")
  (when (or prefix (hash-table-empty-p +jg-vcs-task-hash))
    (clrhash +jg-vcs-task-hash)
    (let* ((root (projectile-project-root))
           (task-buff (get-buffer-create "*gradle*"))
           (tasks-raw (let ((default-directory root)
                            (exec-path (list root)))
                        (with-current-buffer task-buff (erase-buffer))
                        (call-process +jg-vcs-gradle-command nil task-buff nil "tasks" "--all")))
           (tasks-lines (s-lines (with-current-buffer task-buff (buffer-string))))
           (task-hash +jg-vcs-task-hash)
           )
      (cl-loop for line in tasks-lines
               when (s-matches? "^[[:word:]]+ -" line)
               do
               (let ((parts (s-split "-" line t)))
                 (puthash (s-trim (car parts)) (cadr parts) task-hash))
               )
      )
    )
  (let ((task (ivy-read "Choose Task: " +jg-vcs-task-hash :caller 'jg-vcs-gradle-vy))
        (buff (get-buffer-create "*gradle*"))
        (default-directory (projectile-project-root))
        (exec-path (list (projectile-project-root)))
        )
    (with-current-buffer buff (erase-buffer))
    (apply 'call-process +jg-vcs-gradle-command nil buff nil task +jg-vcs-gradle-command-args)
    (+popup-buffer buff)
    )
  )

(defun +jg-vcs-run-gradle-quiet (prefix)
  (interactive "P")
  (let ((+jg-vcs-gradle-command-args (cons "-q" +jg-vcs-gradle-command-args)))
    (+jg-vcs-run-gradle prefix)
    )
  )
