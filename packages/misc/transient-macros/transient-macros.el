;;; transient.el -*- lexical-binding: t; -*-
(require 'transient)

(defconst fmt-as-bool-pair '("T" . "F"))
(defvar transient-quit!
  [
   ""
   [("q" "Quit" transient-quit-one)]
   [("Q" "Quit" transient-quit-all)]
   ]
  " Reusable simple quit for transients "
  )

(defclass jg-transient--group (transient-prefix)
  ((description :initarg :description :initform nil))
  "Prefix Subclassed to hold a description"
  )

(cl-defmethod transient-format-description :before ((obj jg-transient--group))
  "Format the description by calling the next method.  If the result
is nil, then use \"(BUG: no description)\" as the description.
If the OBJ's `key' is currently unreachable, then apply the face
`transient-unreachable' to the complete string."
  (or (funcall (oref obj description))
      (propertize "(JG BUG: no description)" 'face 'error))
)


;;;###autoload
(defun fmt-as-bool! (arg)
  " pass in a value, convert it to one of the values in `fmt-as-bool-pair` "
  (if arg
       (car fmt-as-bool-pair)
     (cdr fmt-as-bool-pair)
     )
  )

;;;###autoload
(defun transient-args! ()
  " utility for easily getting all current transient args "
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
(defmacro transient-make-toggle! (mode &optional key desc heading)
  " Macro to define a transient toggle easier "
  (let* ((fullname (intern (format "jg-transient-toggle-%s" (symbol-name mode))))
         (name (let ((str (or desc (symbol-name mode))))
                 (when heading
                   (put-text-property 0 (length str) 'face 'transient-heading str))
                 str))
        (desc-fn `(lambda () (format "%-2s : %s" (fmt-as-bool! ,mode) ,name)))
        )
    `(progn
       (defvar ,mode nil)
       (transient-define-suffix ,fullname ()
               :transient t
               ,@(when key (list :key key))
               :description ,desc-fn
               (interactive)
               (,mode 'toggle)
               )
       )
     )
  )

;;;###autoload
(cl-defmacro transient-make-subgroup! (name bind docstring &body body &key (desc nil) &allow-other-keys)
  (let ((prefix (gensym))
        (docfn (gensym))
        (doc (let ((str (or desc (symbol-name name))))
                (put-text-property 0 (length str) 'face 'transient-heading str)
                str)))
    (when (keywordp (car body))
      (pop body) (pop body)
      )
    `(progn (transient-define-prefix ,prefix ()
              ,docstring
              ;; :class jg-transient--group
              ,@body
              transient-quit!
              )
            (defun ,docfn nil ,doc)
            (defconst ,name (list ,bind  (quote ,docfn) (quote ,prefix)))
            )
    )
  )

;;;###autoload
(cl-defmacro transient-make-call! (name fmt &body body)
  " create a transient suffix of `name`
with a string or format call, which executes the body
 "
  (let ((fullname (intern (format "jg-transient-call-%s" (if (stringp name) name
                                                           (symbol-name name)))))
        )
    `(transient-define-suffix ,fullname ()
         :transient t
         :description (lambda () ,fmt)
         (interactive)
         ,@body
         )
     )
  )

(provide 'transient-macros)
