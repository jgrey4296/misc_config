;;; jg-list/+funcs.el --- summary -*- lexical-binding: t -*-

;; Simple Functions to feed into sort-subr

;;;###autoload
(defun +jg-lisp-key-start ()
  (re-search-forward "(defun " nil t)
  (symbol-at-point))

;;;###autoload
(defun +jg-lisp-next-rec-end-func ()
  (evil-forward-end 'evil-defun))

;;;###autoload
(defun +jg-lisp-next-rec-func ()
  (evil-forward-beginning 'evil-defun))

;;;###autoload
(defun +jg-lisp-sort-defuns ()
  " A Lisp buffer sorting function "
  (interactive)
  (goto-char (point-min))
  (+jg-lisp-next-rec-func)
  (sort-subr nil
             #'+jg-lisp-next-rec-func
             #'+jg-lisp-next-rec-end-func
             #'+jg-lisp-key-start)
  (goto-char (point-min))
  )

;;;###autoload
(defun +jg-lisp-cleanup-ensure-newline()
  (while (re-search-forward (rx (group ) (opt ";;;###" (+? any) "\n")
                                bol (syntax open-parenthesis)  (| "def" "use") (+? any) eol) nil t)
    (goto-char (match-beginning 1))
    (insert "\n")
    (goto-char (match-end 0))
    (when (< 10000 (line-number-at-pos))
      (signal 'line-overflow (line-number-at-pos))
      )
    )
  )

;;;###autoload
(defun +jg-lisp-insert-signature ()
  " Insert the signature of a function according to helpful "
  (interactive)
  (ivy-read "Insert Function Signature: " obarray
            :predicate (lambda (sym)
                         (or (fboundp sym)
                             (get sym 'function-documentation)))

            :require-match t
            :keymap counsel-describe-map
            :preselect (funcall counsel-describe-function-preselect)
            :action (lambda (x) (insert (helpful--signature (intern x))))
            :caller '+jg-lisp-insert-signature
            )
  )

;;;###autoload
(defun +jg-lisp-pretty-region ()
  (interactive)
  (cond ((eq evil-state 'visual)
         (with-restriction evil-visual-beginning evil-visual-end
           (pp-buffer))
         )
        (t (pp-buffer))
        )
  )

;;;###autoload
(defun +jg-lisp-dired-generate-autoloads ()
  (interactive)
  (let ((outfile (format "%s-autoloads.el"
                         (f-base default-directory))
                 ))
    (loaddefs-generate default-directory outfile)
    )
  )
