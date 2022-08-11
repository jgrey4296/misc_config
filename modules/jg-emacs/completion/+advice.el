;;; +advice.el -*- lexical-binding: t; -*-

;; (define-advice +eval-open-repl (:override (prompt-p &optional displayfn)
;;                                 +jg-repl-filter)
;;   (if (null jg-completion-repl-list)
;;       (setq +eval-repls (-filter #'(lambda (x) (commandp (cdr x)))
;;                                  (cl-loop for sym being the symbols
;;                                           for sym-name = (symbol-name sym)
;;                                           if (string-match "^\\(?:\\+\\)?\\([^/]+\\)/open-\\(?:\\(.+\\)-\\)?repl$" sym-name)
;;                                           collect
;;                                           `(,(format "%s (%s)"
;;                                                      (match-string-no-properties 1 sym-name)
;;                                                      (or (match-string-no-properties 2 sym-name) "default"))
;;                                             . ,(intern sym-name))
;;                                           ))
;;             )
;;     )

;;   (let* ((choice (if (or current-prefix-arg (not (alist-get (symbol-name major-mode) +eval-repls nil nil #'equal)))
;;                      (ivy-read "Open a REPL for: " +eval-repls)
;;                    (symbol-name major-mode)))
;;          (fn (alist-get choice +eval-repls nil nil #'equal))
;;          (region (if (use-region-p)
;;                      (buffer-substring-no-properties (region-beginning)
;;                                                      (region-end))))
;;          )
;;       (unless (commandp fn)
;;         (error "Couldn't find a valid REPL for %s" major-mode))
;;       (with-current-buffer (+eval--ensure-in-repl-buffer fn nil displayfn)
;;         (when (bound-and-true-p evil-mode)
;;           (call-interactively #'evil-append-line))
;;         (when region
;;           (insert region))
;;         t))
;;   )
