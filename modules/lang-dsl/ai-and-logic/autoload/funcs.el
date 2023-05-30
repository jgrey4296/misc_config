;;; +funcs.el -*- lexical-binding: t; -*-

;; TODO prolog rule manager

 ;; |                           |                                            |                                    |                                |             |
 ;; | constraints               | consumers                                  | focus: pred(X)                     | precursors                     | facts.      |
 ;; |---------------------------+--------------------------------------------+------------------------------------+--------------------------------+-------------|
 ;; |                           |                                            |                                    |                                |             |
 ;; | :- pred(X).               | other(X) :- pred(X).                       | pred(X) :- prior(X, Y), prior2(Y). | prior(X,Y) :- a, b, c.         | pred(bob).  |
 ;; | :- pred(X), not other(X). | other(X) :- pred(X), some.                 | pred(X) :- prior(X, Y), prior3(Y). | prior(X)   :- d, e, f.         | pred(bill). |
 ;; |                           |                                            | pred(X) :- prior(X).               |                                |             |
 ;; |                           | another :- pred(_).                        |                                    | prior2(Y)  :- something, else. |             |
 ;; |                           |                                            |                                    |                                |             |
 ;; |                           | different(X) :- some, diff(X, Y), pred(Y). |                                    | prior3(Y)  :- final.           |             |
 ;; |                           |                                            |                                    |                                |             |


;; Also: initial fact list, all heads list, constraints list


;;;###autoload
(defun +jg-pasp-eval (beg end)
  "Evaluate a region and print it to the echo area (if one line long), otherwise
to a pop up buffer."
  (pasp-run-buffer)
)

;;;###autoload
(defun +jg-pasp-extract-elements ()
  (interactive)
  (let ((data (make-hash-table))
        curr-line
        accumulating)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        ;; get line
        (setq curr-line
              (if accumulating
                  (s-trim (concat curr-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                (s-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

        (setq accumulating (not (or (s-matches? "^%" curr-line)
                                    (s-matches? "\\.$" curr-line))))

        (if (not accumulating)
            (progn
              (message "Handling: %s" curr-line)
              (cond ((s-matches? "^%" curr-line)
                     t
                     )
                    ((s-matches? "^[[:blank:]]*:- " curr-line)
                     (push curr-line (gethash :constraint data))
                     )
                    ((s-matches? " :- " curr-line)
                     (push curr-line (gethash :rule data))
                     )
                    ((s-matches? "^#" curr-line)
                     (push curr-line (gethash :instruction data))
                     )
                    (t
                     (push curr-line (gethash :constant data))
                     )
                    )
              (setq curr-line nil)
              )
          )
        (forward-line)
        )
      (with-temp-buffer-window "analysis" 'display-buffer nil
        (cl-loop for key in (hash-table-keys data)
              do
              (princ key) (princ ":\n\n")
              (mapc (lambda (x) (princ x) (princ "\n"))
                    (sort (gethash key data) 'string-lessp))
              (princ "\n-----\n\n")
              )
        )
      )
    )
  )

;;;###autoload
(defun +jg-logic-pasp-indent ()
  (interactive)
  (beginning-of-line)
  (let ((prior-col 0))
    (cond ((looking-at "^.*?:-") ;; match constraint
           (re-search-forward "^[[:blank:]]+" (line-end-position) t)
           (delete-region (line-beginning-position) (point))
           )
          ((and (looking-at ".+?[,\.][[:blank:]]*$") ;; match continued rule
                (save-excursion (forward-line -1)
                                (cond ((re-search-forward ":- " (line-end-position) t)
                                       (setq prior-col (current-column))
                                       t)
                                       ((re-search-forward "^[[:blank:]]+\\(.+?,\\)[[:blank:]]*$" (line-end-position) t)
                                        (goto-char (match-beginning 1))
                                        (setq prior-col (current-column))
                                        t)
                                       (t nil)
                                       )))
           (re-search-forward "^[[:blank:]]+" (line-end-position) t)
           (delete-region (line-beginning-position) (point))
           (indent-to-column prior-col)
           )
          (t
           (re-search-forward "[[:blank:]]*" (line-end-position) t)
           (delete-region (line-beginning-position) (point))
           )
          )
    )
  )
