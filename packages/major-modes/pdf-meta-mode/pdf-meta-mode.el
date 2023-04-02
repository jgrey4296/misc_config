;;; pdf-meta-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: November 18, 2022
;; Modified: November 18, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  to help with adding bookmarks to pdfs
;;
;;; Code:
(require 'evil)
;;-- end header

(defvar pdf-meta--cmd    "pdftk")
(defvar pdf-meta--window "*Pdf-Meta*")

(defun pdf-meta-beginning-of-section (&optional arg)
  (re-search-backward "^\\w+$" nil t arg)
  )

(defun pdf-meta-end-of-section ()
  (forward-line)
  (re-search-forward "^\\w+$" nil t)
  (beginning-of-line)
  (backward-char)
  )

(defun pdf-meta-inc-bookmark-level (&optional arg)
  (interactive)
  (let ((bounds (if (eq evil-state 'visual)
                    (cons evil-visual-beginning evil-visual-end)
                  (bounds-of-thing-at-point 'defun)))
        )
    (save-excursion
      (goto-char (car bounds))
      (while (re-search-forward "BookmarkLevel: " (cdr bounds) t)
        (evil-numbers/inc-at-pt (if arg -1 1) (point))
        )
      )
    )
  )

(defun pdf-meta-dec-bookmark-level ()
  (interactive)
  (pdf-meta-inc-bookmark-level t)
  )

(defun pdf-meta-extract-info ()
  " Create a .info file for each marked pdf "
  (interactive)
  (let ((marked (dired-get-marked-files)))
    (cl-loop for file in marked
             do
             (call-process pdf-meta--cmd nil `((:file ,(concat (f-filename file) ".info")) nil)
                    nil file "dump_data_utf8")
             )
    )
  )

(defun pdf-meta-update-info ()
  " Apply the current info file onto a pdf file "
  (interactive)
  (let* ((fname (buffer-file-name (current-buffer)))
         (target (read-file-name "Apply Onto: "
                                 default-directory
                                 nil t
                                 (f-no-ext (buffer-file-name (current-buffer)))
                                 (-rpartial #'f-ext? "pdf")
                                 ))
         (updated (concat (f-no-ext target) "_updated.pdf"))
         )
    (with-temp-buffer-window pdf-meta--window nil nil
      (call-process pdf-meta--cmd nil pdf-meta--window nil target "update_info_utf8" fname "output" updated)
      ;; (princ (format "Fname: %s\nTarget: %s\nUpdated: %s\n----------\n" fname target updated))
      )
    )
  )

(defun pdf-meta-split ()
  " Split marked pdfs according to a pattern "
  (interactive)
  (let* ((marked (dired-get-marked-files))
         (out_target (read-file-name "Output Dir: " default-directory nil nil "out"))
         stem current
        )
    (unless (f-directory? out_target) (f-mkdir out_target))
    (cl-loop for file in marked
             do
             (setq stem (f-no-ext (f-filename file))
                   current (f-join out_target stem))
             (unless (f-directory? current) (f-mkdir current))
             (call-process pdf-meta--cmd nil nil nil file "burst" "output" (format "%s/%s_%s.pdf" current stem "%04d" ))
             )
    )
  )

(defun pdf-meta-join ()
  "Join marked pdfs together"
  (interactive)
  (let* ((target (read-file-name "Output Target: " default-directory nil nil "out.pdf" (-rpartial #'f-ext? "pdf")))
         (args   (append (dired-get-marked-files) '("cat" "output" ) (list target)))
         )
    (apply #'call-process pdf-meta--cmd nil nil nil args)
    )
  )

(defun pdf-meta-attach ()
  "Attach marked files to a pdf"
  (interactive)
  (let* ((target (read-file-name "Target: " default-directory nil t nil (-rpartial #'f-ext? "pdf")))
         (marked (dired-get-marked-files))
         (args   (append (list target "attach_files") marked (list "output" (format "%s_attached.pdf" (f-no-ext target)))))
         )
    (apply #'call-process pdf-meta--cmd nil nil nil args)
    )
  )

(defun pdf-meta-detach ()
  "Unpack files out of a pdf"
  (interactive)
  (let ((target (read-directory-name "Output Target: " default-directory nil nil "unpacked"))
        (marked (dired-get-marked-files))
        )
    (unless (f-directory? target) (f-mkdir target))
    (cl-loop for file in marked
             do
             (call-process pdf-meta--cmd nil nil nil file "unpack_files" "output" target)
             )
    )
  )

;;-- key-map
(defvar-local pdf-meta-mode-map
  (make-sparse-keymap))

(evil-make-overriding-map pdf-meta-mode-map)

(evil-define-key 'normal pdf-meta-mode-map ">" #'pdf-meta-inc-bookmark-level)
(evil-define-key 'visual pdf-meta-mode-map ">" #'pdf-meta-inc-bookmark-level)
(evil-define-key 'normal pdf-meta-mode-map "<" #'pdf-meta-dec-bookmark-level)
(evil-define-key 'visual pdf-meta-mode-map "<" #'pdf-meta-dec-bookmark-level)
;;-- end key-map

;;-- font-lock
(defconst pdf-meta-font-lock-keywords
  (list
   ;;backquote (,(rx ) (subexp facename override laxmatch))
   `(,(rx line-start (group-n 1 (* word)) ":" (* blank) (group-n 2 (* any)) line-end)
     (1 font-lock-builtin-face)
     (2 font-lock-variable-name-face)
     )
   )
  "Highlighting for pdf-meta-mode"
  )

;;-- end font-lock

(define-derived-mode pdf-meta-mode fundamental-mode
  "pdf-meta"
  (interactive)
  (kill-all-local-variables)
  (use-local-map pdf-meta-mode-map)

  (setq-local forward-sexp-function nil
              beginning-of-defun-function 'pdf-meta-beginning-of-section
              end-of-defun-function 'pdf-meta-end-of-section
              paragraph-start "\\w+$"
              outline-regexp "\\w+$"
              outline-level '(lambda () 0)
              )
  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list pdf-meta-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'pdf-meta-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'pdf-meta-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table pdf-meta-mode-syntax-table)
  ;;
  (setq major-mode 'pdf-meta-mode)
  (setq mode-name "pdf-meta")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  )

(add-to-list 'auto-mode-alist '("\.pdf_meta$" . pdf-meta-mode))

(provide 'pdf-meta-mode)
;;; pdf-meta-mode.el ends here
