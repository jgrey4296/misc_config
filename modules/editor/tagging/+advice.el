;;; +advice.el -*- lexical-binding: t; -*-

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
