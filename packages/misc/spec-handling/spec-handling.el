;;; spec-handling.el -*- lexical-binding: t; no-byte-compile: t;  -*-
(require 'dash)
(require 'cl-lib)
(require 'benchmark)

(defvar spec-handling-hook nil)
(defvar spec-handling-feature-set nil)
(defconst spec-handling-generated-name-plist '(:table "spec-table"
                                               :apply "reapply-specs-fn"
                                               :feature "spec-feature"
                                               :mode-hook "spec-hook-init-fn"
                                               :set "spec-set"
                                               :add "spec-add-delayed-fn"
                                               ))
(defconst spec-handling-symbol-separator "-")

(defun spec-handling--symname (&rest names)
  (intern (string-join (mapcar (lambda (x)
                                 (cond
                                  ((keywordp x)
                                   (plist-get spec-handling-generated-name-plist x))
                                  ((symbolp x)
                                   (symbol-name x))
                                  (t
                                   x))
                                 )
                               names)
                       spec-handling-symbol-separator
                       )
          )
  )

(defun spec-handling-first-run ()
  (message "UI Reapply Spec Hook Firing")
  (provide 'spec-handling-first-run)
  )

;;(add-hook 'spec-handling-hook #'spec-handling-first-run -100)

;;;###autoload
(defun run-spec-handlers ()
  " Run spec handlers defined with spec-handling-new! and spec-handling-add! "
  (interactive)
  (message "Spec Handlers Ran in: %s seconds"
           (benchmark-elapse
             (run-hooks 'spec-handling-hook)
             )
           )
  )

;;;###autoload
(defmacro spec-handling-new! (type target sorted accum-kw &rest body)
  " Simplifies Spec application and definition
body is run for each (key . (vals)) of the spec-table and set as the value of
target
TODO: add spec format docstring
 "
  (cl-assert (-contains? '(collect append do) accum-kw))
  (let ((table-name (spec-handling--symname type :table))
        (reapply-name (spec-handling--symname type :apply))
        (feature-name (spec-handling--symname type :feature))
        (fname (macroexp-file-name))
        )
     `(unless (featurep (quote ,feature-name))
        (defvar ,table-name (make-hash-table :test 'equal),(format "Macro generated hash-table to store specs for %s" type ))
        (fset (function ,reapply-name)
              (lambda (&optional dry)
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
              )

        (add-hook (quote spec-handling-hook) (function ,reapply-name))
        (provide (quote ,feature-name))
        (spec-handling-cleanup-after-provide (quote ,feature-name))
        (quote ,feature-name)
        )
     )
  )

;;;###autoload
(defmacro spec-handling-new-hooks! (type &rest body)
  " register handlers for given modes adapted from doom' set-rotate-patterns! "
  (let ((table-name (spec-handling--symname type :table))
        (reapply-name (spec-handling--symname type :apply))
        (feature-name (spec-handling--symname type :feature))
        (fname (macroexp-file-name))
        )
    `(unless (-contains? spec-handling-hook (function ,reapply-name))
       (defvar ,table-name nil  ,(format "Macro generated hash-table to store specs for %s" type ))
       (setq ,table-name (make-hash-table :test 'equal))
       (fset (quote ,reapply-name)
             (lambda (&optional dry)
               ,(format "Macro-Generated spec application fn for: %s\n from: %s" type fname)
               (interactive)
               (cl-loop for modes being the hash-keys of ,table-name
                        using (hash-values val)
                        do
                        (cl-loop for key in (ensure-list modes)
                                 do
                                 (message "Making %s Hook for %s" (quote ,type) key)
                                 (let ((fn-name (spec-handling--symname (quote ,type) key :mode-hook)))
                                   (fset fn-name
                                         (-partial (lambda (val)
                                                     ,@body
                                                     )
                                                   val)
                                         )
                                   (add-hook (intern (format "%s-hook" key)) fn-name)
                                   )
                                 )
                        )
               )
             )
       (add-hook (quote spec-handling-hook) (function ,reapply-name))
       (provide (quote ,feature-name))
       (spec-handling-cleanup-after-provide (quote ,feature-name))
       (quote ,feature-name)
       )
    )
  )

;;;###autoload
(defmacro spec-handling-add! (type override &rest rules)
  (let* ((fname (macroexp-file-name))
         (table-name (spec-handling--symname type :table))
         (feature-name (spec-handling--symname type :feature))
         (add-fn-name (spec-handling--symname type fname :add))
         )
    `(with-eval-after-load (quote ,feature-name)
       (cl-loop for val in (quote ,rules)
                ,@(if override '(do)
                    `(if (gethash (car val) ,table-name nil)
                         do (message "Spec Handling Add: Attempt to override Spec: %s - %s - %s" (quote ,type) (car val) ,fname)
                         else do))
                (puthash (car val) (cdr val) ,table-name)
                )
         )
    )
  )

;;;###autoload
(defmacro spec-handling-setq! (type &rest vals)
  (let ((set-name (spec-handling--symname type :set)))
    `(progn
       (fset (function ,set-name) (lambda () (setq ,@vals)))
       (add-hook 'spec-handling-hook (function ,set-name) 99)
      )
    )
  )

;;;###autoload
(defmacro spec-handling-extend! (type &rest rules)
  " extend a registered spec with more rules "
  )

;;;###autoload
(defmacro spec-handling-clear! (type)
  (let ((table-name (spec-handling--symname type :table))
        (reapply-name (spec-handling--symname type :apply))
        (feature-name (spec-handling--symname type :feature))
        )
    `(progn
       (when (boundp (quote ,table-name)) (clrhash ,table-name) (unintern (quote ,table-name) nil))
       (remove-hook 'spec-handling-hook (function ,reapply-name))
       (unintern (function ,reapply-name) nil)
       (spec-handling-cleanup-after-provide (quote ,feature-name))
       (setq features (remove (quote ,feature-name) features))
       (quote ,feature-name)
       )
    )
  )

(defun spec-handling-cleanup-after-provide (sym)
  (interactive "x")
  (setq after-load-alist (--remove (equal (car it) sym) after-load-alist))
  nil
  )

(provide 'spec-handling)

;; company
;; eval-handler
