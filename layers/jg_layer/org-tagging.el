(provide 'jg_layer/org-setup-tagging)

(require 'helm)

(defun jg_layer/org-setup-tagging ()
  """ Setup Helm Tagging, but to be called in init.d
        so both org and helm are set up """

  (defun jg_layer/org-set-tags (x)
    """ Toggle Selected Tags """
    (let* ((visual-candidates (helm-marked-candidates))
           (actual-candidates (mapcar (lambda (x) (cadr (assoc x jg_layer/org-tagging-candidates))) visual-candidates))
           (current-tags (org-get-tags nil t)))
      (mapc (lambda (candidate)
              (if (not (-contains? current-tags candidate))
                  (push candidate current-tags)
                (setq current-tags (remove candidate current-tags))
                )) actual-candidates)
      (org-set-tags current-tags)
      ))

  (defun jg_layer/org-set-new-tag (x)
    (let ((current-tags (org-get-tags nil t)))
      (if (not (-contains? current-tags x))
          (push x current-tags))
      (org-set-tags current-tags)
    ))

  (defun jg_layer/sort-candidates (ap bp)
    """ Sort candidates by colour then lexicographically """
    (let* ((a (car ap))
	      (b (car bp))
	      (aprop (get-text-property 0 'font-lock-face a))
          (bprop (get-text-property 0 'font-lock-face b))
          (lookup (lambda (x) (gethash (cadr x) jg_layer/org-tagging-candidate-counts))))
      (cond
       ((and aprop bprop (> (funcall lookup ap) (funcall lookup bp))) t)
       ((and aprop (not bprop)) t)
       ((and (not aprop) (not bprop) (> (funcall lookup ap) (funcall lookup bp))))
       )))

  (defun jg_layer/org-tagging-candidates ()
    """ Given Candidates, colour them if they are assigned, then sort them  """
    (let* ((cand-counts (jg_layer/org-count-buffer-tags)))
      (if (hash-table-keys cand-counts)
          (let* ((cand-keys (hash-table-keys cand-counts))
                 (cand-vals (hash-table-values cand-counts))
                 (cand-pairs (-zip cand-keys cand-vals))
                 (maxTagLength (apply 'max (mapcar 'length cand-keys)))
                 (bar-keys (jg_layer/make-bar-chart cand-pairs maxTagLength))
                 (display-pairs (-zip bar-keys cand-keys))
                 (current-tags (org-get-tags nil t))
                 (propertied-tags (map 'list (lambda (candidate)
                                               (let ((candString (car candidate)))
                                                 (if (-contains? current-tags (cdr candidate))
                                                     (progn (put-text-property 0 (length candString)
                                                                               'font-lock-face
                                                                               'rainbow-delimiters-depth-1-face
                                                                               candString)))
                                                 `(,candString ,(cdr candidate)))) display-pairs))
                 )
            (setq jg_layer/org-tagging-candidate-counts cand-counts)
            (setq jg_layer/org-tagging-candidates (sort propertied-tags 'jg_layer/sort-candidates))
            )
        '()
        ))
      )

  ;; The Two Sources For Tagging Helm
  (setq jg_layer/org-tagging-candidates '()
        jg_layer/org-tagging-candidate-counts '()
        jg_layer/org-tagging-helm `((name . "Helm Tagging")
                                    (action . jg_layer/org-set-tags)
                                    )
        jg_layer/org-tagging-fallback-source (helm-build-dummy-source ""
                                               :action '((helm-pattern . jg_layer/org-set-new-tag)))
        )

  (defun jg_layer/org-tagging-helm-start ()
    """ Opens the Tagging Helm """
    (interactive)
    (let* ((candidates (jg_layer/org-tagging-candidates))
           (main-source (cons `(candidates . ,(mapcar 'car candidates)) jg_layer/org-tagging-helm)))
      (helm :sources '(main-source jg_layer/org-tagging-fallback-source)
            :input "")
      )
    )
  )
