(provide 'jg_layer/org-setup-tagging)

(require 'helm)

(defun jg_layer/org-setup-tagging ()
  """ Setup Helm Tagging, but to be called in init.d
        so both org and helm are set up """

  (defun jg_layer/org-set-tags (x)
    """ Toggle Selected Tags """
    (let* ((visual-candidates (helm-marked-candidates))
           (actual-candidates (mapcar (lambda (x) (cadr (assoc x jg_layer/org-tagging-candidates-names))) visual-candidates))
           (prior-point 1)
           (end-line(cdr jg_layer/org-tagging-region))
           (current-tags '())
           (add-func (lambda (candidate)
                       (if (not (-contains? current-tags candidate))
                           (push candidate current-tags)
                         (setq current-tags (remove candidate current-tags))
                         ))))
      (save-excursion
        (goto-char (car jg_layer/org-tagging-region))
        (setq prior-point (- (point) 1))
        (while (and (/= prior-point (point)) (< (line-number-at-pos (point)) end-line))
          (progn (setq current-tags (org-get-tags nil t)
                       prior-point (point))
                 (mapc add-func actual-candidates)
                 (org-set-tags current-tags)
                 (org-forward-heading-same-level 1)
                 )))))

  (defun jg_layer/org-set-new-tag (x)
    (save-excursion
      (goto-char (car jg_layer/org-tagging-region))
      (let ((prior-point (- (point) 1))
            (end-line(cdr jg_layer/org-tagging-region)))
        (while (and (/= prior-point (point)) (< (line-number-at-pos (point)) end-line))
          (setq prior-point (point))
          (let* ((current-tags (org-get-tags nil t)))
            (if (not (-contains? current-tags x))
                (push x current-tags))
            (org-set-tags current-tags)
            (org-forward-heading-same-level 1)
            )))))

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
      (if (not (hash-table-empty-p cand-counts))
          (let* ((cand-keys (hash-table-keys cand-counts))
                 (cand-vals (hash-table-values cand-counts))
                 (cand-pairs (-zip cand-keys cand-vals))
                 (maxTagLength (apply 'max (mapcar 'length cand-keys)))
                 (maxTagAmount (apply 'max cand-vals))
                 (bar-keys (jg_layer/make-bar-chart cand-pairs maxTagLength maxTagAmount))
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
            (setq jg_layer/org-tagging-candidates-names (sort propertied-tags 'jg_layer/sort-candidates))
            )
        '()
        ))
    )

  ;; The Two Sources For Tagging Helm
  (setq jg_layer/org-tagging-candidates-names '()
        jg_layer/org-tagging-candidate-counts '()
        ;; Start Position -> End Line number because of changes in positions from tag add/retract
        jg_layer/org-tagging-region '()
        jg_layer/org-tagging-helm `((name . "Helm Tagging")
                                    (action . jg_layer/org-set-tags)
                                    )
        jg_layer/org-tagging-fallback-source (helm-build-dummy-source ""
                                               :action '((helm-pattern . jg_layer/org-set-new-tag)))
        )

  (evil-define-operator jg_layer/org-tagging-helm-start (beg end)
    """ Opens the Tagging Helm """
    (interactive "<R>")
    (setq jg_layer/org-tagging-region `(,beg . ,(line-number-at-pos end)))
    (let* ((candidates (jg_layer/org-tagging-candidates))
           (main-source (cons `(candidates . ,(mapcar 'car candidates)) jg_layer/org-tagging-helm)))
      (helm :sources '(main-source jg_layer/org-tagging-fallback-source)
            :input "")
      )
    )
  )
