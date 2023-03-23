;;; tagging-helm.el -*- lexical-binding: t; -*-

(defvar tagging-minor-mode-helm-source)
(defvar tagging-minor-mode-fallback-source)
(defvar tagging-minor-mode-helm-buffer-name  "*Helm Tags*")

(define-advice helm-grep--prepare-cmd-line (:override (only-files &optional include zgrep)
                                            +jg-tagging-grep-helm-override)
  (let* ((default-directory (or helm-ff-default-directory
                                (helm-default-directory)
                                default-directory))
         (fnargs            (helm-grep-prepare-candidates
                             only-files default-directory))
         (ignored-files     (unless (helm-grep-use-ack-p)
                              (mapconcat (lambda (x) (concat "--exclude=" (shell-quote-argument x)))
                               helm-grep-ignored-files " ")))
         (ignored-dirs      (unless (helm-grep-use-ack-p)
                              (mapconcat (lambda (x) (concat "--exclude-dir=" (shell-quote-argument x)))
                               helm-grep-ignored-directories " ")))
         (exclude           (unless (helm-grep-use-ack-p)
                              (let ((inc     (and include (concat include " ")))
                                    (igfiles (and ignored-files (concat ignored-files " ")))
                                    (igdirs  (and helm-grep-in-recurse ignored-dirs)))
                                (concat inc igfiles igdirs))))
         (types             (and (helm-grep-use-ack-p) (or include "")))
         (smartcase         (if (helm-grep-use-ack-p) ""
                              (unless (let ((case-fold-search nil))
                                        (string-match-p "[[:upper:]]" helm-pattern))
                                "i")))
         (helm-grep-default-command (concat helm-grep-default-command " %m"))
         (pipe-switches (mapconcat 'identity helm-grep-pipe-cmd-switches " "))
         (patterns (helm-mm-split-pattern helm-pattern t))
         (pipcom (concat " | " (helm-grep--pipe-command-for-grep-command smartcase pipe-switches "grep")))
         (pipes (if (cdr patterns)
                    (concat pipcom (s-join pipcom (mapcar #'shell-quote-argument (cdr patterns))))
                  ""))
         ;; (patterns-alt (s-join " -e " (mapcar #'shell-quote-argument patterns)))
         (cmd (format-spec helm-grep-default-command
                           (delq nil
                                 (list (unless zgrep
                                         (if types
                                             (cons ?e types)
                                           (cons ?e exclude)))
                                       (cons ?c (or smartcase ""))
                                       (cons ?p (shell-quote-argument (car patterns)))
                                       ;; (cons ?p patterns-alt)
                                       (cons ?f fnargs)
                                       (cons ?m pipes)
                                       ;; (cons ?m "")
                                       ))))
         )
    cmd
    )
  )

(define-advice helm-grep--pipe-command-for-grep-command (:override (smartcase pipe-switches &optional grep-cmd)
                                                         +jg-tagging-helm-ggrep-fix)
  (pcase (or grep-cmd (helm-grep-command))
    ;; Use grep for GNU regexp based tools.
    ((or "grep" "zgrep" "git-grep")
     (format "grep --color=always%s %s"
             (if smartcase " -i" "") pipe-switches))
    ("ggrep"
     (format "grep --color=always%s %s"
             (if smartcase " -i" "") pipe-switches))
    ((and (pred (string-match-p "ack")) ack)
     (format "%s --smart-case --color %s" ack pipe-switches)))
  )

(defun +jg-tag-sort-candidates (ap bp)
  " Sort routine to sort by colour then lexicographically "
  (let* ((a (car ap))
         (b (car bp))
         (aprop (get-text-property 0 'font-lock-face a))
         (bprop (get-text-property 0 'font-lock-face b))
         (lookup (lambda (x) (gethash (cadr x) jg-tag-global-tags))))
    (cond
     ((and aprop bprop (> (funcall lookup ap) (funcall lookup bp))) t)
     ((and aprop (not bprop)) t)
     ((and (not aprop) (not bprop) (> (funcall lookup ap) (funcall lookup bp))))
     )))
(defun +jg-tag-candidates (current-tags)
  " Given Candidates, colour them if they are assigned, then sort them  "
  (let* ((buffer-cand-tags (+jg-tag-get-buffer-tags))
         (global-tags jg-tag-global-tags))
    (if (not (hash-table-empty-p global-tags))
        (let* ((cand-keys (hash-table-keys global-tags))
               (cand-vals (hash-table-values global-tags))
               (cand-pairs (-zip-pair cand-keys cand-vals))
               (maxTagLength (apply 'max (mapcar 'length cand-keys)))
               (maxTagAmount (apply 'max cand-vals))
               (bar-keys (+jg-text-make-bar-chart cand-pairs maxTagLength maxTagAmount))
               (display-pairs (-zip-pair bar-keys cand-keys))
               (propertied-tags (cl-map 'list (lambda (candidate)
                                             (let ((candString (car candidate)))
                                               (if (-contains? current-tags (cdr candidate))
                                                   (progn (put-text-property 0 (length candString)
                                                                             'font-lock-face
                                                                             'rainbow-delimiters-depth-1-face
                                                                             candString)))
                                               `(,candString ,(cdr candidate)))) display-pairs))
               )
          (setq jg-tag-candidate-counts global-tags)
          (setq jg-tag-candidates-names (sort propertied-tags '+jg-tag-sort-candidates))
          )
      '()
      ))
  )
(defun +jg-tag-chart-tag-counts (counthash name)
  "Given a hashtable of counts, create a buffer with a bar chart of the counts"
  ;; (message "Charting: %s %s" counthash name)
  (let* ((hashPairs (-zip-pair (hash-table-keys counthash) (hash-table-values counthash)))
         (sorted (sort hashPairs (lambda (a b) (> (cdr a) (cdr b)))))
         (maxTagLength (apply 'max (mapcar (lambda (x) (length (car x))) sorted)))
         (maxTagAmnt (apply 'max (mapcar (lambda (x) (cdr x)) sorted)))
         )
    ;;print them all out

    (with-temp-buffer-window "*Tags*"
                             nil
                             nil
                             ;; Todo: Expand this func to group and add org headings
                             (mapc (lambda (x) (princ (format "%s\n" x)))
                                   (+jg-text-make-bar-chart sorted maxTagLength maxTagAmnt))
                             )
    (+jg-tag-org-format-temp-buffer "*Tags*" name)
    )
  )

(defun +jg-tag-set-tags-re-entrant (x)
  (unless (s-equals? (s-trim (car x)) jg-tag-re-entrant-exit-tag)
    (+jg-tag-set-tags x)
    (with-helm-buffer
      (setq-local helm-input-local " ")
      )
    (helm-resume jg-tag-helm-buffer-name)
    )
  )

(defun +jg-tag-set-new-tag-re-entrant (x)
  (unless (s-equals? (s-trim x) jg-tag-re-entrant-exit-tag)
    (+jg-tag-set-new-tag x)
    (with-helm-buffer
      (setq-local helm-input-local " ")
      )
    (helm-resume jg-tag-helm-buffer-name)
    )
  )
