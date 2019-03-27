;; jg_emacs funcs.el
;; loaded third.

;; (when (configuration-layer/package-usedp 'package)
;;   (defun spacemacs/<package>-enable () )
;;   (defun spacemacs/<package>-disable () ))


;;----------------------------------------
(when (configuration-layer/package-usedp 'auto-complete)
  (defun jg_layer/ac-trigger ()
    (interactive)
    (auto-complete)
    )
  )

;;----------------------------------------
(when (configuration-layer/package-usedp 'org)
  (defun jg_layer/open_link_in_buffer ()
    """ a util function to force links to be open in emacs  """
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

  (defun jg_layer/make-bar-chart (data maxTagLength)
    ;; TODO Scale within 80 columns
    (mapcar (lambda (x)
              (string-join `(,(car x)
                             ,(make-string (- (+ 10 maxTagLength) (length (car x))) ?\ )
                             ": "
                             ,(number-to-string (cdr x))
                             ,(make-string (- 5 (length (number-to-string (cdr x)))) ?\ )
                             " : "
                             ,(make-string (cdr x) ?=)
                             ;; "\n"
                             ))) data))



  (defun jg_layer/org-count-buffer-tags ()
    (save-excursion ;;store where you are in the current
      (goto-char (point-min))
      ;;where to store tags:
      (let ((tag-set (make-hash-table :test 'equal)))
        ;;match all
        (while (not (eq nil (re-search-forward ":\\([[:graph:]]+\\):\\(\.\.\.\\)?\$" nil t)))
          ;;split tags into list
          (let* ((tags (split-string (match-string-no-properties 0) ":" t ":"))
                 (filtered (seq-filter (lambda (x) (not (or (string-equal x "PROPERTIES") (string-equal x "END")))) tags)))
            ;;increment counts
            (mapc (lambda (x) (puthash x (+ 1 (gethash x tag-set 0)) tag-set)) filtered)
            )
          )
        tag-set
        )
      )
    )

  (defun jg_layer/tag-occurences-in-open-buffers()
    """ retrieve all tags in all open buffers, print to a temporary buffer """
    (interactive)
    (let* ((allbuffers (buffer-list))
           (alltags (make-hash-table :test 'equal))
           (hashPairs nil)
           (sorted '())
           (maxTagLength 0))
      (map 'list (lambda (bufname)
                   ;; TODO quit on not an org file
                   (with-current-buffer bufname
                     (let ((buftags (jg_layer/org-count-buffer-tags)))
                       (maphash (lambda (k v)
                                  (puthash k (+ v (gethash k alltags 0)) alltags))
                                buftags)
                       ))) allbuffers)
      (setq hashPairs (-zip (hash-table-keys alltags) (hash-table-values alltags)))
      (if hashPairs (progn
                     (setq sorted (sort hashPairs (lambda (a b) (> (cdr a) (cdr b)))))
                     (setq maxTagLength (apply `max (mapcar (lambda (x) (length (car x))) sorted)))))
      (with-temp-buffer-window "*Tags*"
                               nil
                               nil
                               (mapc (lambda (x) (princ (format "%s\n" x)))
                                     (jg_layer/make-bar-chart sorted maxTagLength))
                               )
      )
    )

  (defun jg_layer/tag-occurances ()
    """ Count all occurrences of all tags and bar chart them """
    (interactive)
    ;;save eventually to a new buffer
    (let ((tag-set (jg_layer/org-count-buffer-tags))
          (hashPairs nil)
          (sorted '())
          (maxTagLength 0))
      (setq hashPairs (-zip (hash-table-keys tag-set) (hash-table-values tag-set)))
      (if hashPairs (progn
                      (setq sorted (sort hashPairs (lambda (a b) (> (cdr a) (cdr b)))))
                      (setq maxTagLength (apply `max (mapcar (lambda (x) (length (car x))) sorted)))))
      ;;print them all out
      (with-temp-buffer-window "*Tags*"
                               nil
                               nil
                               (mapc (lambda (x) (princ (format "%s\n" x)))
                                     (jg_layer/make-bar-chart sorted maxTagLength)))
      )
    )

  )

;;--------------------------------------------------
(defun jg_layer/insert-lparen ()
  """ utility to insert a (  """
  (interactive)
  (insert "(")
  )

(defun jg_layer/insert-rparen ()
  """ utility to insert a ) """
  (interactive)
  (insert ")")
  )

(defun jg_layer/flatten (lst)
  """ Utility to flatten a list """
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
  """ Utility to clear a buffer
    from https://stackoverflow.com/questions/24565068/ """
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer))
  )

;;----------------------------------------

(defun jg_layer/example_transient_func_setup ()
  """ Create an example transient state """

  (defun jg_layer/example_transient_func ()
    """ The simple function to call from within the transient state """
    (interactive)
    (print "blah")
    )

  (spacemacs|define-transient-state example_transient
    """ Define the transient state itself """
    :title "an example transient state"
    :doc (concat "
[_q_]^^ Quit         [_h_]^^ Example Func
[_t_]^^ Example Func
")
    :bindings
    ("q" nil :exit t)
    ("t" jg_layer/example_transient_func)
    ("h" jg_layer/example_transient_func :exit t)
    )

  ;; Then register its activation
  (spacemacs/set-leader-keys "." 'spacemacs/example_transient-transient-state/body)
  )

;;----------------------------------------
(defun jg_layer/example-helm-setup ()

  (setq-default jg_layer/example_helm-source
                (helm-make-source "Find Image" 'helm-source-ffiles
                  :action '(("action" . (lambda (candidate) (insert candidate))))))

  (defun jg_layer/example_helm-call ()
    (interactive)
    (helm :sources jg_layer/example_helm-source
          :input "./"))
  )

