;; jg_emacs funcs.el
;; loaded third.

;; (when (configuration-layer/package-usedp 'package)
;;   (defun spacemacs/<package>-enable () )
;;   (defun spacemacs/<package>-disable () ))

(when (configuration-layer/package-usedp 'org)
  ;;call occur for all tags in the file:
  (defun jg_layer/tag-occurances ()
    (interactive)
    ;;save eventually to a new buffer
    (with-output-to-temp-buffer "*tags*"
      (save-excursion ;;store where you are in the current
        (goto-char (point-min))
        ;;where to store tags:
        (let ((tag-set (make-hash-table :test 'equal)))
          ;;match all
          (while (not (eq nil (re-search-forward ":\\([[:graph:]]+\\):\\(\.\.\.\\)?\$" nil t)))
            ;;split tags into list
            (let ((tags (split-string (match-string-no-properties 0) ":" t ":")))
              ;;increment counts
              (mapc (lambda (x) (puthash x (+ 1 (gethash x tag-set 0)) tag-set)) tags)
              )
            )
          ;;now turn them into pairs
          (let ((hashPairs nil) (sorted '()) (maxTagLength 0))
            (maphash (lambda (k v) (push `(,k ,v) hashPairs)) tag-set)
            (setq sorted (sort hashPairs (lambda (a b) (string-lessp (car a) (car b)))))
            (setq maxTagLength (apply `max (mapcar (lambda (x) (length (car x))) sorted)))
            ;;print them all out
            (mapc (lambda (x)
                    (princ (string-join `(,(car x)
                                          ,(make-string (- (+ 10 maxTagLength) (length (car x))) ?\ )
                                          ": "
                                          ,(number-to-string (cadr x))
                                          ,(make-string (- 5 (length (number-to-string (cadr x)))) ?\ )
                                          " : "
                                          ,(make-string (cadr x) ?=)
                                          "\n"
                                          ))))
                  sorted)
            )
          )
        )
      )
    )
  )


(defun jg_layer/insert-lparen ()
  (interactive)
  (insert "(")
  )

(defun jg_layer/insert-rparen ()
  (interactive)
  (insert ")")
  )


(defun jg_layer/flatten (lst)
  (letrec ((internal (lambda (x)
                       (cond
                        ((null x) nil)
                        ((atom x) (list x))
                        (t
                         (append (funcall internal (car x)) (funcall internal (cdr x))))))))
    (progn
      (assert (listp lst))
      (funcall internal lst))))
