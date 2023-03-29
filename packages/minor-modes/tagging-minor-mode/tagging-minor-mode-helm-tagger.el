;;; tagging-helm.el -*- lexical-binding: t; -*-

(require 'evil)
(require 'helm-source)
(require 'helm-grep)
(require 'helm-utils)
(require 'helm-files)

(defvar tagging-minor-mode--helm-source)
(defvar tagging-minor-mode--fallback-source)
(defvar tagging-minor-mode--helm-buffer-name  "*Helm Tags*")
(defvar tagging-minor-mode-re-entrant-quit-char "|")

(defvar tagging-minor-mode-candidate-counts '())
(defvar tagging-minor-mode-candidate-names '())

(defun tagging-minor-mode--trim-input (x)
  (let ((trimmed (string-trim x)))
    (s-replace-regexp "\s+" "_" trimmed)
    )
  )

(define-advice helm-grep--prepare-cmd-line (:override (only-files &optional include zgrep)
                                            tagging-minor-mode-grep-helm-override)
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
                                                         tagging-minor-mode-helm-ggrep-fix)
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

(defun tagging-minor-mode-insert-candidates (x)
  "A Helm action to insert selected candidates into the current buffer "
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;Substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (substring x 0 -2)) candidates "\n")))))

(defun tagging-minor-mode--sort-candidates (ap bp)
  " Sort routine to sort by colour then lexicographically "
  (let* ((a (car ap))
         (b (car bp))
         (aprop (get-text-property 0 'font-lock-face a))
         (bprop (get-text-property 0 'font-lock-face b))
         (lookup (lambda (x) (gethash (cadr x) tagging-minor-mode-global-tags))))
    (cond
     ((and aprop bprop (> (funcall lookup ap) (funcall lookup bp))) t)
     ((and aprop (not bprop)) t)
     ((and (not aprop) (not bprop) (> (funcall lookup ap) (funcall lookup bp))))
     )))

(defun tagging-minor-mode-candidates (current-tags)
  " Given Candidates, colour them if they are assigned, then sort them  "
  (let* ((global-tags tagging-minor-mode-global-tags))
    (if (not (hash-table-empty-p global-tags))
        (let* ((cand-keys (hash-table-keys global-tags))
               (cand-vals (hash-table-values global-tags))
               (cand-pairs (-zip-pair cand-keys cand-vals))
               (maxTagLength (apply 'max (mapcar 'length cand-keys)))
               (maxTagAmount (apply 'max cand-vals))
               (bar-keys (tagging-minor-mode--make-bar-chart cand-pairs maxTagLength maxTagAmount))
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
          (setq tagging-minor-mode-candidate-counts global-tags)
          (setq tagging-minor-mode-candidate-names (sort propertied-tags 'tagging-minor-mode--sort-candidates))
          )
      '()
      ))
  )

(defun tagging-minor-mode-set-tags-re-entrant (x)
  (unless (s-equals? (s-trim (car x)) tagging-minor-mode-re-entrant-quit-char)
    (tagging-minor-mode-set-tags x)
    (with-helm-buffer
      (setq-local helm-input-local " ")
      )
    (helm-resume tagging-minor-mode--helm-buffer-name)
    )
  )

(defun tagging-minor-mode-set-new-tag-re-entrant (x)
  (unless (s-equals? (s-trim x) tagging-minor-mode-re-entrant-quit-char)
    (tagging-minor-mode-set-new-tag x)
    (with-helm-buffer
      (setq-local helm-input-local " ")
      )
    (helm-resume tagging-minor-mode--helm-buffer-name)
    )
  )

(setq tagging-minor-mode--helm-source
      (helm-make-source "Helm Tagging" 'helm-source
        :action (helm-make-actions "Re-entrant-set" #'tagging-minor-mode-set-tags-re-entrant
                                   "Set"            #'tagging-minor-mode-set-tags)
        :pattern-transformer #'tagging-minor-mode--trim-input
        )
      )
(setq tagging-minor-mode--fallback-source
      (helm-build-dummy-source "Helm Tags Fallback Source"
        :action (helm-make-actions "Re-entrant-Create" #'tagging-minor-mode-set-new-tag-re-entrant
                                   "Create"            #'tagging-minor-mode-set-new-tag)
        :filtered-candidate-transformer (lambda (_c _s) (list helm-pattern)))
      )

(defun tagging-minor-mode-tagger (&optional beg end)
  " Opens the Tagging Helm "
  (interactive)
  (unless tagging-minor-mode
    (error "Tagging Minor Mode not active")
    )
  (set-marker tagging-minor-mode-marker (if (eq evil-state 'visual) evil-visual-end (line-end-position)))
  (get-buffer-create tagging-minor-mode--helm-buffer-name)

  (let* ((current-tags (tagging-minor-mode-get-tags))
         (candidates   (tagging-minor-mode-candidates current-tags))
         (main-source (cons `(candidates . ,candidates) tagging-minor-mode--helm-source))
         )
    (helm :sources (list main-source tagging-minor-mode--fallback-source)
          :input ""
          :buffer tagging-minor-mode--helm-buffer-name
          )
    )
  )

(evil-ex-define-cmd "t[ag]"  #'tagging-minor-mode-tagger)
(provide 'tagging-minor-mode-helm-tagger)
