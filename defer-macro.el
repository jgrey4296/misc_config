;;; defer-macro.el -*- lexical-binding: t; -*-

(defmacro dlog! (text &rest args)
  " A Simple, doom-less debug message when 'debug-on-error is true"
  `(when debug-on-error
     (let ((inhibit-message t))
       (funcall #'message ,text ,@args)
       )
     )
  )

(defun defer--load-files (dir &rest files)
  "Load files with a debug log message"
  (cl-loop for file in files
           do
           (dlog! "Deferred Loading: %s : %s" dir file)
           (let ((fname (file-name-concat dir file)))
             (load fname nil (file-exists-p (concat fname ".el")))
             )
           )
  )

(defmacro defer-load! (&optional afters &rest files)
  "set a timer to load the given files after 5 seconds"
  (let ((core-timer `(run-with-idle-timer (+ 4 (random 4)) nil
                      #'defer--load-files
                      (dir!)
                      ,@(if (stringp afters) (cons afters files) files)
                      ))
        temp
        )
    (cond ((symbolp afters) '(val)
           (list #'with-eval-after-load (list 'quote afters)
                 core-timer))
          ((consp afters)
           (setq temp core-timer)
           (while afters
             (setq temp (append (list #'with-eval-after-load (list 'quote (pop afters)))
                                (list temp))))
             temp)
          (t (list 'progn core-timer))
          )
    )
  )

(defmacro defer! (time &rest body)
  `(run-with-idle-timer ,time nil
    (lambda ()
      ,@body
      )
    )
  )

(defun debug-func--print-return (fn x)
  (message "Fn: %s : %s" fn x)
  )

(defun debug-func! (fn)
  (advice-add fn :filter-return (-partial #'debug-func--print-return fn))
  )

(defun undebug-func! (fn)
  (advice-mapc (lambda (func props) (advice-remove fn func))
               fn
               )
  )
