;; jg_emacs funcs.el
;; loaded third.

;; (when (configuration-layer/package-usedp 'package)
;;   (defun spacemacs/<package>-enable () )
;;   (defun spacemacs/<package>-disable () ))

(when (configuration-layer/package-usedp 'auto-complete)
  (defun jg_layer/ac-trigger ()
    (interactive)
    (auto-complete)
    )
  )


(when (configuration-layer/package-usedp 'org)
  (defun jg_layer/open_link_in_buffer ()
    (interactive)
    (org-open-at-point 'in-emacs)
    )


  (defun jg_layer/list-agenda-files ()
    """ Creates a temporary, Org-mode buffer with links to agenda files """
    (interactive)
    (with-output-to-temp-buffer "*Agenda Files*"
      (set-buffer "*Agenda Files*")
      (insert "Agenda Files: ")
      (insert "\n")
      (mapc (lambda (x)
              (let ((file_name (last (split-string x "/" t ".org"))))
              (insert (format "[[%s][%s]]\n" x file_name))
              )) org-agenda-files)
      (org-mode)
      )
    )

  (defun jg_layer/tag-occurences-in-open-buffers()
    """ retrieve all tags in all open buffers, print to a temporary buffer """
    ;; TODO enable use on subset of buffers, or list of links
    ;; get all open buffers

    ;; create a map of [tag -> (buffers)]

    ;; print into a new temp buffer
    ;; ;; sideways bar chart of [tag_link_to_tag_files_list -> count ]
    ;; ;; A Heading with links to the files
    (print "Not Implemented Yet")
    ;; buffer-list -> filter -> use
    )

  (defun jg_layer/tag-occurances ()
    """ call occur for all tags in the file """
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


(defun jg_layer/clear-buffer ()
    " from https://stackoverflow.com/questions/24565068/ "
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer))
    )
