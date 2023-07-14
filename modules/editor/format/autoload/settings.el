;;; editor/format/autoload/settings.el -*- lexical-binding: t; -*-

;; This must be redefined here because `format-all' only makes it available at
;; compile time.

(defconst +format-system-type
  (cl-case system-type
    (windows-nt 'windows)
    (cygwin     'windows)
    (darwin     'macos)
    (gnu/linux  'linux)
    (berkeley-unix
     (save-match-data
       (let ((case-fold-search t))
         (cond ((string-match "freebsd" system-configuration) 'freebsd)
               ((string-match "openbsd" system-configuration) 'openbsd)
               ((string-match "netbsd"  system-configuration) 'netbsd))))))
  "Current operating system according to the format-all package.")

(defun +format--resolve-system (choices)
  "Get first choice matching `format-all-system-type' from CHOICES."
  (cl-loop for choice in choices
           if (atom choice) return choice
           else if (eql +format-system-type (car choice))
           return (cadr choice)))

(defun +format--make-command (formatter &rest _)
  `(format-all--buffer-thunk
    (lambda (input)
      (with-silent-modifications
        (setq buffer-file-name ,(buffer-file-name (buffer-base-buffer))
              default-directory ,default-directory)
        (delay-mode-hooks (funcall ',major-mode))
        (insert input)
        (condition-case e
            (progn
              (doom-log "formatter (commandp) %s" #',formatter)
              (call-interactively #',formatter)
              (list nil ""))
          (error (list t (error-message-string e))))))))

(defun +format--make-function (formatter &rest _)
  `(progn
     (doom-log "formatter (functionp) %s" #',formatter)
     (format-all--buffer-thunk #',formatter)))

(defun +format--make-shell-command (command ok-statuses error-regexp)
  (+format--make-shell-command-list (split-string command " " t)
                                    ok-statuses error-regexp))

(defun +format--make-shell-command-list (command-list ok-statuses error-regexp)
  `(let (args)
     (dolist (arg ',command-list)
       (cond ((stringp arg)
              (push arg args))
             ((listp arg)
              (catch 'skip
                (let (subargs this)
                  (while (setq this (pop arg))
                    (cond ((not (stringp (car arg)))
                           (let ((val (eval (pop arg) t)))
                             (unless val (throw 'skip nil))
                             (push (format this val) subargs)))
                          ((stringp this)
                           (push this subargs))))
                  (setq args (append subargs args)))))))
     (doom-log "formatter (arglist) %s" args)
     (if ,(and (or ok-statuses error-regexp) t)
         (apply #'format-all--buffer-hard
                ',ok-statuses ,error-regexp nil
                (reverse args))
       (apply #'format-all--buffer-easy (reverse args)))))

(cl-defun +format--set (name &key function modes unset)
  (declare (indent defun))
  (when (and unset (not (gethash name format-all--format-table)))
    (error "'%s' formatter does not exist to be unset" name))
  (puthash name function format-all--format-table)
  (dolist (mode (ensure-list modes))
    (cl-destructuring-bind (m &optional probe)
        (ensure-list mode)
      (if unset
          (puthash m (assq-delete-all name (gethash key format-all-mode-table))
                   format-all-mode-table)
        (format-all--pushhash
         m (cons name (if probe `(lambda () ,probe)))
         format-all--mode-table)))))
