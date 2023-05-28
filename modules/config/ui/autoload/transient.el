;;; transient.el -*- lexical-binding: t; -*-
(require 'transient)

(defvar fmt-as-bool-pair '("T" . "F"))
(defvar transient-quit!
  [
   ""
   ("q" "Quit" transient-quit-one)
   ]
  " Reusable simple quit for transients "
  )

;;;###autoload
(defun fmt-as-bool (arg)
  (if arg
       (car fmt-as-bool-pair)
     (cdr fmt-as-bool-pair)
     )
  )

;;;###autoload
(defun transient-args! ()
  " utility for easily getting either all current transient args "
      (transient-args transient-current-command)
  )

;;;###autoload
(defun transient-args? (&optional key)
  "utility for easily testing the args in transient"
  (member (if (symbolp key) (symbol-name key) key) (transient-args transient-current-command))
  )

;;;###autoload
(defun transient-init! (arg)
  " utility for simply setting a transient init value "
  (-partial #'(lambda (val obj)
                (oset obj value val))
            arg)
  )

;;;###autoload
(defmacro transient-make-toggle! (mode &optional key desc)
  (let ((fullname (intern (format "jg-transient-toggle-%s" (symbol-name mode))))
        (name (or desc (symbol-name mode)))
        )
    `(progn
       (defvar ,mode nil)
       (transient-define-suffix ,fullname ()
               :transient t
               ,@(when key (list :key key))
               :description (lambda () (format "%-2s : %s" (fmt-as-bool ,mode) ,name))
               (interactive)
               (,mode 'toggle)
               )
       )
     )
  )

;;;###autoload
(cl-defmacro transient-make-call! (mode fmt &body body)
  (let ((fullname (intern (format "jg-transient-call-%s" (symbol-name mode))))
        )
    `(transient-define-suffix ,fullname ()
         :transient t
         :description (lambda () ,fmt)
         (interactive)
         ,@body
         )
     )
  )
