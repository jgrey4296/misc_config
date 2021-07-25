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
          (group-n 1 (+ (or digit ?,)) (? ".BCE"))
          (? blank (group-n 2 "->")
             blank (group-n 3 (+ (or digit ?,)) (? ".BCE")))
          ;; Event Desc
          (+ blank) (group-n 4 (regexp "\".+?\""))
          ;; country
          (+ blank) (group-n 5 (+ alnum))
          ;; People
          (? (* blank) (group-n 6 (* blank (+ word) ?_ (+ word))))
          ;; Tags
          (? (+ blank) (group-n 7 ?: "tags")
             blank (group-n 8 (+ (or word ?,))))
          ;; Wiki
          (? (+ blank) (group-n 9 ?: "link")
             blank (group-n 10 (+ (not blank))))
          ;; Desc
          (? blank (group-n 11 ?: "desc"))
          line-end
          )
     (1 "font-lock-builtin-face")
     (2 "font-lock-constant-face"      nil t)
     (3 "font-lock-builtin-face"       nil t)
     (4 "font-lock-type-face")
     (5 "font-lock-function-name-face")
     (6 "font-lock-variable-name-face" nil t)
     (7 "org-document-info"            nil t)
     (8 "org-list-dt"                  nil t)
     (9 "org-document-info"            nil t)
     (10 "org-link"                    nil t)
     (11 "org-document-info"           nil t))

   `("^.+$" (0 font-lock-warning-face))
   )
  "Highlighting for timeline-mode"
  )

(defun timeline-indent-line ()
  ;; TODO
  )

(defun timeline-align-line ()
  ;; TODO
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
  ;; (set (make-local-variable 'comment-start) "#")
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
