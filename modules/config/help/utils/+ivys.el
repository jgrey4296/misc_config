;;; +ivys.el -*- lexical-binding: t; -*-

(defvaralias 'jg-help-module-dirs 'doom-modules-dirs)
(defvar jg-help-module-cache-file nil)
(defvar jg-help-module-find-cmd (executable-find "rg"))
(defvar jg-help-module-times (make-hash-table))

(defun jg-help-module-load-cache ()
  " reconstruct hash table jg-help-module-times "
  ;; read  jg-help-module-cache-file
  ;; insert into jg-help-module-times
  )

(defun +jg-help-update-modules-status ()
  " update jg-help-module-times "
  (unless (t) ;; last time updated was today
  (with-temp-buffer
    (cl-loop for root in jg-help-module-dirs
             for loc  in (f-directories root)
             do
             (erase-buffer)
             ;; Most recent files
             (call-process jg-help-module-find-cmd nil t nil "--sortr" "modified" "--files" loc)
             ;; mod time of first file
             ;; if newer than jg-help-module-times:
             ;; mark in hash table
             )
        )
  )
)

(defun +jg-help-modules-ivy ()
  " List modules, annotating with active or not, and last modified time "
  (interactive)
  (+jg-help-update-modules-status)
  (ivy-read "Module: " jg-help-module-times)

  )
