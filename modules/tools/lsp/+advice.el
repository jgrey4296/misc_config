;;; +advice.el -*- lexical-binding: t; -*-

(define-advice +eval--ensure-in-repl-buffer (:filter-return (result) +jg-repl-fix)
  (message "Repl Result: %s" result)
  (if (bufferp result)
      (with-current-buffer result
        (let* ((mode (intern-soft (s-replace "inferior-" "" (symbol-name major-mode))))
               (mode2 (intern-soft (format "inferior-%s" mode)))
               (project-root (doom-project-root))
              )
          (if (and mode (not (gethash (cons mode project-root) +eval-repl-buffers)))
              (puthash (cons mode project-root) result +eval-repl-buffers))
          (if (and mode2 (not (gethash (cons mode2 project-root) +eval-repl-buffers)))
              (puthash (cons mode2 project-root) result +eval-repl-buffers))
          )
        )
    )
  result
  )


(define-advice +jg-send-region-to-repl (:filter-args (args) +jg-advice-send-repl-auto-line)
  " Handle visual-mode variance for send-region-to-repl "
  (if (not (eq evil-state 'visual))
      (list (line-beginning-position) (line-end-position) (if (eq (length args) 3)  (last args) nil))
      args)
  )
