;;; prompts.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header


;;;###autoload
(defun +jg-snippets-yas-prompt-fn (prompt choices &optional display-fn)
  " Yasnippet ivy which shows groups "
  (let* ((max-name 0)
         (max-group -1)
         (index 0)
         ;; Get the bits i care about to create the display
         (choice-parts (cl-loop for template in choices
                                collect
                                (let ((name (s-replace "\\" "/" (s-trim (yas--template-name template))))
                                      (group (s-trim (apply 'concat (yas--template-group template))))
                                      (table (concat "Table:" (yas--table-name (yas--template-table template))))
                                      )
                                  (cl-incf index)
                                  (setq max-name (max (length name) max-name)
                                        max-group (max (length group) max-group))
                                  (list (list name (length name)) (list group (length group)) table (1- index)))))
         (format-fn (lambda (x) `(,(concat " "
                                           ;; template name + gap
                                           (caar x) (make-string (- (+ 5 max-name) (cadar x)) ? ) " : "
                                           ;; groups
                                           (caadr x) (make-string (- (+ 5 max-group) (cadadr x)) ? ) " : "
                                           ;; table
                                           (caddr x))
                                  ;; index
                                  . ,(car (last x)))))
         (chosen nil)
         )
    ;; only once i know the max-name, format the choices
    ;; use :action rather than return value, to use the (str . index) pair
    (ivy-read "Choose Snippet: " (mapcar format-fn choice-parts) :caller '+jg-snippets-yas-ivy :action (lambda (x) (setq chosen (cdr x))))
    (nth (or chosen 0) choices)
    )
  )

;;;###autoload
(defun +jg-snippets-str-prompt-fn (prompt choices &optional display-fn)
  "Choice function for when choices aren't templates"
  (if (-all? #'yas--template-p choices)
      nil
    (yas-completing-prompt prompt choices)
    )
  )



;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 23, 2025
;; Modified:   September 23, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 30.2))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; prompts.el ends here
