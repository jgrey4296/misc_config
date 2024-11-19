;;; +spec-defs.el -*- lexical-binding: t; -*-

(defvar-local company-backends-sort-vals nil)

(defvar jg-company-known-backends '(
                                    company-abbrev
                                    company-anaconda
                                    company-auctex-bibs
                                    company-auctex-environments
                                    company-auctex-labels
                                    company-auctex-macros
                                    company-auctex-symbols
                                    company-bbdb
                                    company-capf
                                    company-clang
                                    company-cmake
                                    company-coq-dynamic-symbols-backend
                                    company-coq-dynamic-tactics-backend
                                    company-coq-generic-refman-backend
                                    company-coq-generic-snippets-backend
                                    company-coq-local-definitions-backend
                                    company-coq-pg-backend
                                    company-coq-user-snippets-backend
                                    company-dabbrev
                                    company-dabbrev-code
                                    company-dict
                                    company-etags
                                    company-faces
                                    company-files
                                    company-gtags
                                    company-ispell
                                    company-keywords
                                    company-latex-commands
                                    company-lua
                                    company-math-symbols-latex
                                    company-math-symbols-unicode
                                    company-mlton
                                    company-nxml
                                    company-oddmuse
                                    company-restclient
                                    company-semantic
                                    company-shell
                                    company-shell-env
                                    company-web-html
                                    company-web-jade
                                    company-web-slim
                                    company-yasnippet
                                    )
  "Known backends for use or trying"
)

(defconst jg-company-backend-position-default 60)

(defconst jg-company-backend-positions '(:front     1
                                         :favour    25
                                         :mode      50
                                         :disfavour 75
                                         :back      89
                                         :last      100
                                         )
  "available priorities/positions for placing company backends")

(defun +jg-company-position-parse (val)
  "Convert (cons :priority 'backend) into (cons int 'backend)"
  (cond ((not (consp val))
         (cons jg-company-backend-position-default val))
        ((integerp (car val))
         val)
        ((and (keywordp (car val)) (plist-get jg-company-backend-positions (car val)))
         (cons (plist-get  jg-company-backend-positions (car val))
               (cdr val)))
        (t
         (cons jg-company-backend-position-default val))
        )
  )

(spec-handling-new! company nil :loop 'hook
                    :doc "Registers hooks for specific modes which set the company backends"
                    :struct '(list (priority backends*)*)
                    ;; Create val sort if missing
                    (unless (and company-backends-sort-vals (hash-table-p company-backends-sort-vals))
                      (setq-local company-backends-sort-vals (make-hash-table)))
                    ;; Group backends by priority:
                    (mapc #'(lambda (x) (let ((pval (+jg-company-position-parse x)))
                                          (mapc
                                           (-partial #'(lambda (priority backend)
                                                         (cl-pushnew backend
                                                                     (gethash priority company-backends-sort-vals)))
                                                     (car pval))
                                           (ensure-list (cdr pval)))))
                          val)

                    ;; Set actual backends to each (:separate *group)
                    (setq-local company-backends
                                (cl-loop for key in (sort (hash-table-keys company-backends-sort-vals) #'<)
                                         collect
                                         (cons :separate (gethash key company-backends-sort-vals))
                                         )
                                )
                      )
