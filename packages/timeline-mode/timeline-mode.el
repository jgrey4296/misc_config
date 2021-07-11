;;; timeline-mode.el -*- lexical-binding: t; -*-

(require 'timeline-utilities)

(defvar-local timeline-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst timeline-font-lock-keywords
  (list
   '("^#.+$" (0 'font-lock-comment-face))
   ;; Year start and end
   `(,(rx line-start
          (group-n 1 (+ digit) blank)
          (? (group-n 2 "->" blank)
             (group-n 3 (+ digit) blank))
          )
     (1 "font-lock-builtin-face")
     (2 "font-lock-constant-face")
     (3 "font-lock-builtin-face")
     )
   ;; Event description and country
   `(,(rx (group-n 4 (regexp "\".+?\"") (? blank))
          (group-n 5 (+ alnum)  (? blank)))
     (4 "font-lock-type-face")
     (5 "font-lock-function-name-face"))
   ;; People
   `(,(rx (? (group-n 6 (* (+ word) ?_ (+ word) (? blank))))) ;; 6: people
     (6 "font-lock-variable-name-face"))
                                                              ;; Tags
   `(,(rx (? (group-n 7 ?: "tags" blank)                      ;; 7: tag header
             (group-n 8 (+ (or word ?,)))                     ;; 8: tags
             (? blank)))
     (7 "org-document-info")
     (8 "org-list-dt")
     )
                                                              ;; Wiki
   `(,(rx (? (group-n 9 ?: "wiki") blank                      ;; 9: wiki header
             (group-n 10 (+ (not blank)))
             (? (or line-end blank))                          ;; 10: wiki link
             ))
     (9 "org-document-info")
     (10 "org-link")
     )
                                                              ;; Descr
   `(,(rx (? (group-n 11 ?: "desc"))                          ;; 11: desc header
          line-end)
     (11 "org-document-info"))

   `("^.+$" . font-lock-warning-face)
   )
  "Highlighting for timeline-mode"
  )


(define-derived-mode timeline-mode fundamental-mode
  "timeline"
  "For Editing Timeline Specs"
  (interactive)
  (kill-all-local-variables)
  (use-local-map timeline-mode-map)

  (set (make-local-variable 'font-lock-defaults) (list timeline-font-lock-keywords t))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) nil)
  ;; (set (make-local-variable 'indent-line-function) 'timeline-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table timeline-mode-syntax-table)
  ;;
  (setq major-mode 'timeline-mode)
  (setq mode-name "timeline")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  )
(add-to-list 'auto-mode-alist '("\\.timeline" . timeline-mode))

(provide 'timeline-mode)
