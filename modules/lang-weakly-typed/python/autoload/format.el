;;; format.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python-cleanup-import-blocks ()
  " Collect all ##-- imports blocks,
and move them to the start of the file,
then sort them
TODO
  "
  (interactive)
  (let ((source (current-buffer))
        (collected-imports (make-temp-file "collected-imports"))
        start end groupname cleaned)
    ;; Go from bottom of buffer to top
    (with-current-buffer source
      (goto-char (point-max))
      (while (re-search-backward (code-shy-fold-block-gen :re t) nil t)
        (setq groupname (match-string 1))
        (cond ((and (s-matches? "^imports" groupname)
                    end
                    (not start))
               (beginning-of-line)
               (setq start (point)))
              ((and (s-matches? "^end imports" groupname)
                    (not end))
               (end-of-line)
               (setq end (point))))
        ;; Copy the block to the temp buffer
        (if (and start end)
            (progn
              (-if-let (folds (vimish-fold--folds-in start end))
                  (vimish-fold--delete (car folds)))
              (goto-char end)
              (insert "\n")
              (write-region start (+ 1 end) collected-imports t)
              (kill-region start end)
              (setq start nil
                    end nil)
              (end-of-line -0)
            )
          (end-of-line -0)
          )
        )
      )
    ;; Then cleanup the collect imports
    (with-temp-buffer
      (insert-file-contents collected-imports)
      (goto-char (point-min))
      (flush-lines "##-- ")
      (write-file collected-imports)
      (py-isort-buffer)
      (setq cleaned (s-trim (buffer-string)))
      (write-file collected-imports)
      )
    ;; And Insert back into original buffer
    (with-current-buffer source
      (goto-char (point-min))
      (re-search-forward "^\"\"\"" nil t)
      (re-search-forward "^\"\"\"" nil t)
      (end-of-line)
      (insert "\n")
      (insert (code-shy-fold-block-gen :name "imports" :newlines t))
      (insert cleaned)
      (insert (code-shy-fold-block-gen :name "imports" :newlines t :end t))
      )
    )
  )

;;;###autoload
(defun +jg-python-cleanup-ensure-newline-before-def ()
  (while (re-search-forward "\\(\n\\)\\(\s*@.+?\n\\)*\s*\\(def\\|class\\)" nil t)
    (goto-char (match-end 1))
    (insert "\n")
    (goto-char (match-end 0))
    )
  )

;;;###autoload
(defun +jg-python-align-dictionaries ()
  ;; TODO
  )

;; Simple Functions to feed into sort-subr

(defun +__jg-python-key-start ()
  (re-search-forward "^\s*def ")
  (symbol-at-point))

(defun +__jg-python-next-rec-end-func ()
  (python-nav-forward-sexp))

(defun +__jg-python-next-rec-func ()
  ;; Move Forward
  (beginning-of-defun -1)
  ;; Handle dectorators
  (while (er--python-looking-at-decorator -1)
    (forward-line -1)
    )
  )

;;;###autoload
(defun +jg-python-sort-class-methods ()
  (interactive)
  (+jg-python-select-class)
  (narrow-to-region evil-visual-beginning evil-visual-end)
  (goto-char (point-min))
  (evil-normal-state)
  ;; narrow to class
  (+__jg-python-next-rec-func)
  (sort-subr nil
             #'+__jg-python-next-rec-func
             #'+__jg-python-next-rec-end-func
             #'+__jg-python-key-start)
  (goto-char (point-min))
  (widen)
  )
