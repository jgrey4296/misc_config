;;; ob-clingo.el --- org-babel functions for prolog evaluation.

;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 20, 2022
;; Modified: July 20, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.1))
;;
;; Adapted from:
;; URL: https://github.com/ljos/ob-prolog

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-babel support for clingo.
;;
;; To activate ob-clingo add the following to your init.el file:
;;
;;  (add-to-list 'load-path "/path/to/ob-clingo-dir")
;;  (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((clingo . t)))
;;
;; It is unnecessary to add the directory to the load path if you
;; install using the package manager.
;;
;; In addition to the normal header arguments ob-prolog also supports
;; the :goal argument.  :goal is the goal that prolog will run when
;; executing the source block.  Prolog needs a goal to know what it is
;; going to execute.
;;

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'prolog)

(add-to-list 'org-babel-tangle-lang-exts '("prolog" . "pl"))
(add-to-list 'auto-mode-alist '("\\.lp$" . pasp-mode))

(defvar org-babel-clingo-command  "clingo"
  "Name of the clingo executable command.")
(defconst org-babel-header-args:clingo
  '((:goal . :any))
  "Clingo-specific header arguments.")

(defun org-babel-execute:clingo (body params)
  "Execute the Clingo in BODY according to the block's header PARAMS.

This function is called by `org-babel-execute-src-block.'"
  (message "executing Clingo source code block")
  (let* ((result-params (cdr (assq :result-params params)))
         (session (cdr (assq :session params)))
         (goal (org-babel-clingo--parse-goal
                (cdr (assq :goal params))))
         (vars (org-babel-variable-assignments:clingo params))
         (full-body (org-babel-expand-body:generic body params vars))
         (results (if (string= "none" session)
                      (org-babel-clingo-evaluate-external-process
                       goal full-body params)
                    (message "Clingo doesn't work as a session"))))
    (unless (string= "" results)
      (org-babel-reassemble-table
       (org-babel-result-cond result-params
         results
         (let ((tmp (org-babel-temp-file "clingo-results-")))
           (with-temp-file tmp (insert results))
           (org-babel-import-elisp-from-file tmp)))
       (org-babel-pick-name (cdr (assq :colname-names params))
                            (cdr (assq :colnames params)))
       (org-babel-pick-name (cdr (assq :rowname-names params))
                            (cdr (assq :rownames params)))))))

(defun org-babel-clingo-evaluate-external-process (goal body params)
  "Evaluate the GOAL given the BODY in an external Clingo process.

If no GOAL is given, the GOAL is replaced with HALT.  This results in
running just the body through the Clingo process."
  (let* ((tmp-file (org-babel-temp-file "clingo-"))
         (clingo-params (org-babel-clingo-format-args params))
         (command (format "%s %s %s"
                          org-babel-clingo-command
                          clingo-params
                          tmp-file)))
    (message "All Params: %s" params)
    (with-temp-file tmp-file
      (insert (org-babel-chomp body)))
    (message "Going to execute: %s" command)
    (org-babel-clingo-eval command "")))

(defun org-babel-clingo-eval (cmd body)
  "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'.

NOTE: CLINGO DOESN'T USE NORMAL EXIT CODES
see: https://www.mat.unical.it/aspcomp2013/files/aspoutput.txt
"
  (let ((err-buff (get-buffer-create "*Org-Babel Error*")) exit-code)
    (with-current-buffer err-buff (erase-buffer))
    (with-temp-buffer
      (insert body)
      (setq exit-code
            (org-babel--shell-command-on-region cmd err-buff))
        ;; exit code handling:
      (cond ((not (numberp exit-code))
             (with-current-buffer err-buff
               (org-babel-eval-error-notify exit-code (buffer-string)))
             nil)

            ((< 0 (logand exit-code 64))
             (org-babel-eval-error-notify exit-code (buffer-string))
             "Syntax Error")

            ((< 0 (logand exit-code 2))
             (buffer-string))

            ((< 0 (logand exit-code 1))
             (org-babel-eval-error-notify exit-code (buffer-string))
             "Error")

            ((format "FALLBACK: %s\n\n%s" exit-code (buffer-string)))))))

(defun org-babel-clingo--answer-correction (string)
  "If STRING is Clingo's \"Correct to:\" prompt, send a refusal."
  (when (string-match-p "Correct to: \".*\"\\?" string)
    (comint-send-input nil t)))
(defun org-babel-clingo--exit-debug (string)
  "If STRING indicates an exception, continue Clingo execution in no debug mode."
  (when (string-match-p "\\(.\\|\n\\)*Exception.* \\? $" string)
    (comint-send-input nil t)))
(defun org-babel-clingo-format-args (params)
  " Adapt the parameters passed in into clingo CLI arguments "
  (mapconcat (lambda (x)
               (let ((sym (symbol-name (car x)))
                     (val (or (cdr x) ""))
                     )
                 (cond ((equal ":clingo-args" sym)
                        (cdr x))
                       ((s-prefix? ":c-" sym)
                        (format "%s %s " (s-replace ":c-" "-" sym) val))
                       ((s-prefix? ":clingo-" sym)
                        (format "%s %s " (s-replace ":clingo-" "--" sym) val))
                       (""))
                 )
               )
             params "")
  )
(defun org-babel-clingo--elisp-to-pl (value)
  "Convert the Emacs Lisp VALUE to equivalent Clingo."
  (cond ((stringp value)
         (format "'%s'"
                 (replace-regexp-in-string
                  "'" "\\'" value)))
        ((listp value)
         (format "[%s]"
                 (mapconcat #'org-babel-clingo--elisp-to-pl
                            value
                            ", ")))
        (t (prin1-to-string value))))
(defun org-babel-clingo--variable-assignment (pair)
  "Return a string of a recorda/2 assertion of (cdr PAIR) under (car PAIR).

The Emacs Lisp value of the car of PAIR is used as the Key argument to
recorda/2 without modification.  The cdr of PAIR is converted to
equivalent Clingo before being provided as the Term argument to
recorda/2."
  (format ":- recorda('%s', %s)."
          (car pair)
          (org-babel-clingo--elisp-to-pl (cdr pair))))
(defun org-babel-variable-assignments:clingo (params)
  "Return the babel variable assignments in PARAMS.

PARAMS is a quasi-alist of header args, which may contain
multiple entries for the key `:var'.  This function returns a
list of the cdr of all the `:var' entries."
  (let (vars)
    (dolist (param params vars)
      (when (eq :var (car param))
        (let ((var (org-babel-clingo--variable-assignment (cdr param))))
          (setq vars (cons var vars)))))))

(defun org-babel-clingo--parse-goal (goal)
  "Evaluate the inline Emacs Lisp in GOAL.

Example:
      append(=(+ 2 3), =(quote a), B)
   => append(5, a, B)"
  (when goal
    (with-temp-buffer
      (insert goal)
      (while (search-backward "=" nil t)
        (delete-char 1 t)
        (let ((value (eval
                      (read
                       (thing-at-point 'sexp)))))
          (kill-sexp)
          (insert (format "%S" value))))
      (buffer-string))))


(provide 'ob-clingo)
;;; ob-clingo.el ends here
