;; -*- no-byte-compile: t; -*-
;;; config/bindings/test/fix.el

(evil-define-motion jg-evil-goto-line (count)
  "Go to line COUNT. By default the last line."
  :jump t
  :type line
  (if (null count)
      (with-no-warnings (end-of-buffer))
    (goto-char (point-min))
    (forward-line (1- count)))
  (evil-first-non-blank))

(defmacro jg-evil-ensure-column (&rest body)
  "Execute BODY so that column after execution is correct.
If `evil-start-of-line' is nil, treat BODY as if it were a `next-line' command.
This mostly copies the approach of Emacs' `line-move-1', but is modified
so it is more compatible with Evil's notion of EOL tracking."
  (declare (indent defun) (debug t))
  `(progn
     (unless evil-start-of-line
       (setq ;; this-command 'next-line
             temporary-goal-column
             (cond ((memq last-command '(next-line previous-line))
                    temporary-goal-column)
                   ((and track-eol (eolp) (not (bolp))) most-positive-fixnum)
                   (t (current-column)))))
     ,@body
     (if evil-start-of-line
         (evil-first-non-blank)
       (line-move-to-column
        (or goal-column
            (if (consp temporary-goal-column)
                ;; Guard against a negative value as `temporary-goal-column'
                ;; may have a negative component when both `whitespace-mode'
                ;; and `display-line-numbers-mode' are enabled (#1297).
                (max 0 (+ (truncate (car temporary-goal-column))
                          (cdr temporary-goal-column)))
              temporary-goal-column))))))

(defun jg-evil-escape-fix (fn &rest args)
  (let ((cmd this-command))
    (apply fn args)
    (when (not (eq cmd this-command))
      (setq this-command cmd))
    )
  )
(defun jg-evil-escape-fix2 (&rest args)
  nil
  )

(advice-add 'evil-goto-line :override #'jg-evil-goto-line)
(advice-add 'evil-ensure-column :override #'jg-evil-ensure-column)

(progn
  (advice-add 'evil-escape--insert-func :around #'jg-evil-escape-fix)
  (advice-add 'evil-escape--delete-func :around #'jg-evil-escape-fix)
  )

(advice-add 'evil-escape--insert-func :override #'jg-evil-escape-fix2)
(advice-add 'evil-escape--delete-func :override #'jg-evil-escape-fix2)

(defun jg-evil-undo-fixes ()
  (advice-remove 'evil-goto-line  #'jg-evil-goto-line)
  (advice-remove 'evil-ensure-column #'jg-evil-ensure-column)
  (advice-remove 'evil-escape--insert-func  #'jg-evil-escape-fix)
  (advice-remove 'evil-escape--delete-func  #'jg-evil-escape-fix)
  (advice-remove 'evil-escape--insert-func  #'jg-evil-escape-fix2)
  (advice-remove 'evil-escape--delete-func  #'jg-evil-escape-fix2)
  )
