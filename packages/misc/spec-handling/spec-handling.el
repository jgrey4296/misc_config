;;; spec-handling.el -*- lexical-binding: t; -*-

(defvar spec-handling-hook nil)

(defun spec-handling--symname (&rest names)
  (intern (s-join "-" (mapcar (lambda (x)
                                (if (symbolp x)
                                    (symbol-name x)
                                  x))
                              names)
                  )
          )
  )

(defun spec-handling-first-run ()
  (message "UI Reapply Spec Hook Firing")
  (provide 'spec-handling-first-run)
  )

(add-hook 'spec-handling-hook #'spec-handling-first-run -100)

;;;###autoload
(defun run-spec-handlers ()
  " Run spec handlers defined with spec-handling-new! and spec-handling-add! "
  (interactive)
  (run-hooks 'spec-handling-hook)
  )

;;;###autoload
(defmacro spec-handling-new! (type target sorted accum-kw &rest body)
  " Simplifies Spec application and definition
body is run for each (key . (vals)) of the spec-table and set as the value of
target
TODO: add spec format docstring
 "
  (cl-assert (-contains? '(collect append) accum-kw))
  (let ((table-name (spec-handling--symname type 'spec-table))
        (reapply-name (spec-handling--symname type 'reapply-specs-fn))
        (feature-name (spec-handling--symname type 'spec-feature))
        (fname (macroexp-file-name))
        )
     `(unless (-contains? spec-handling-hook (function ,reapply-name))
        (defvar ,table-name (make-hash-table) ,(format "Macro generated hash-table to store specs for %s" type ))

        (defun ,reapply-name (&optional dry)
          ,(format "Macro-Generated spec application fn for: %s\n from: %s" type fname)
          (interactive)
          (let ((vals (cl-loop for key being the hash-keys of ,table-name
                               using (hash-values val)
                               ,accum-kw
                               ,@body
                               )))
            ,(when sorted
                `(setq vals (mapcar #'cdr
                                    (sort vals #'(lambda (x y) (< (car x) (car y))))
                                    )
                  )
                )
            (unless dry
              (setq ,target (-reject #'null vals)))
            vals
            )
          )

        (add-hook (quote spec-handling-hook) (function ,reapply-name))
        (provide (quote ,feature-name))
        )
     )
  )

;;;###autoload
(defmacro spec-handling-add! (type override &rest rules)
  (let ((table-name (spec-handling--symname type 'spec-table))
        (feature-name (spec-handling--symname type 'spec-feature))
        (fname (macroexp-file-name))
        )
    `(eval-after-load (quote ,feature-name)
      (cl-loop for val in (quote ,rules)
               ,@(if override
                     '(do)
                   `(unless (gethash (car val) ,table-name nil) do))
               (puthash (car val) (cdr val) ,table-name)
               )
      )
    )
  )

;;;###autoload
(defmacro spec-handling-setq! (type &rest vals)
  (let ((feature-name (spec-handling--symname type 'spec-feature))
        (set-name (spec-handling--symname type 'spec-set))
        )
    `(progn
       (defun ,set-name nil (setq ,@vals))
       (add-hook 'spec-handling-hook (function ,set-name) 99)
      )
    )
  )

;;;###autoload
(defmacro spec-handling-extend! (type &rest rules)

  )

(defmacro spec-handling-clear! (type)
  (let ((table-name (spec-handling--symname type 'spec-table))
        (reapply-name (spec-handling--symname type 'reapply-specs-fn))
        )
    `(progn
      (setq ,table-name (make-hash-table :test 'equal))
      (remove-hook 'spec-handling-hook (function ,reapply-name))
      )
    )
  )

(provide 'spec-handling)

;; browse
;; company
;; tagging tagging-minor-mode-add-spec
;; eval-handler

;;rotate-patterns
