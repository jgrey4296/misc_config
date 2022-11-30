;;; test-company-backend.el -*- lexical-binding: t; -*-

(setq test-company-kws '("i8" "u8" "i16" "u16" "i32" "u32" "i64" "u64" "i128" "u128" "isize" "usize" "f32" "f64" "bool" "char")

      )

(defun test-company/backend (cmd &rest args)
  (interactive (list 'interactive))
  (cl-case cmd
    (init            nil)
    ;; Get the value to complete
    (prefix  (when (or (s-matches? ":type" (current-word)) (-contains? test-company-kws (current-word))) (current-word)))
    ;; Get candidates of completion
    (candidates      test-company-kws)
    ;; Defaults
    (sorted          t)
    (duplicates      nil)
    (ignore-case     t)
    (no-cache        t)
    ;; Documentation location:
    (doc-buffer      nil)
    ;; Location of candidate definition
    (location        nil)
    ;; Add data in completion window
    (annotation      nil)
    ;; Add data in echo
    (meta            nil)
    ;; For Late expansion of snippets etc
    (post-completion nil)
    ;; For easy use of backend:
    (interactive     (company-begin-backend 'test-company/backend))
    ;; Difference between usage / creation:
    (require-match   nil)

    (t               nil)
    )
  )


(setq company-backends '(test-company/backend))
