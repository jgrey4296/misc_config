;;; ob-pipelang.el --- org-babel functions for prolog evaluation.

;; Adapted from:
;; Copyright (C) Bjarte Johansen
;; Author: Bjarte Johansen
;; Keywords: literate programming, reproducible research
;; Package-Version: 20190410.2130
;; URL: https://github.com/ljos/ob-prolog
;; Version: 1.0.2

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

;; Org-babel support for pipelang.
;;
;; To activate ob-pipelang add the following to your init.el file:
;;
;;  (add-to-list 'load-path "/path/to/ob-pipelang-dir")
;;  (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((pipelang . t)))
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

(add-to-list 'org-babel-tangle-lang-exts '("pipelang" . "pipe"))
(defvar org-babel-pipelang-command  "pipelang"
  "Name of the pipelang executable command.")
(defconst org-babel-header-args:pipelang
  '((:goal . :any))
  "Pipelang-specific header arguments.")

(defun org-babel-execute:pipelang (body params)
  "Execute the Pipelang in BODY according to the block's header PARAMS.

This function is called by `org-babel-execute-src-block.'"
  (message "executing Pipelang source code block")
  (let* ((result-params (cdr (assq :result-params params)))
         (session (cdr (assq :session params)))
         (goal (org-babel-pipelang--parse-goal
                (cdr (assq :goal params))))
         (vars (org-babel-variable-assignments:pipelang params))
         (full-body (org-babel-expand-body:generic body params vars))
         (results (if (string= "none" session)
                      (org-babel-pipelang-evaluate-external-process
                       goal full-body params)
                    (org-babel-pipelang-evaluate-session
                     session goal full-body))))
    (unless (string= "" results)
      (org-babel-reassemble-table
       (org-babel-result-cond result-params
         results
         (let ((tmp (org-babel-temp-file "pipelang-results-")))
           (with-temp-file tmp (insert results))
           (org-babel-import-elisp-from-file tmp)))
       (org-babel-pick-name (cdr (assq :colname-names params))
                            (cdr (assq :colnames params)))
       (org-babel-pick-name (cdr (assq :rowname-names params))
                            (cdr (assq :rownames params)))))))

(defun org-babel-pipelang-evaluate-session (session goal body)
  "In SESSION, evaluate GOAL given the BODY of the Pipelang block.
Create SESSION if it does not already exist."
  (error "Pipelang session not yet implemented")
  (let* ((session (org-babel-pipelang-initiate-session session))
         (body (split-string (org-babel-trim body) "\n")))
    (with-temp-buffer
      (apply #'insert (org-babel-pipelang--session-load-clauses session body))
      (if (save-excursion
            (search-backward "ERROR: " nil t))
          (progn
            (save-excursion
              (while (search-backward "|: " nil t)
                (replace-match "" nil t)))
            (search-backward "true." nil t)
            (kill-whole-line)
            (org-babel-eval-error-notify -1 (buffer-string))
            (buffer-string))
        (when goal
          (kill-region (point-min) (point-max))
          (apply #'insert
                 (org-babel-comint-with-output (session "")
                   (insert (concat goal ", !."))
                   (comint-send-input nil t))))
        (if (not (save-excursion
                   (search-backward "ERROR: " nil t)))
            (let ((delete-trailing-lines t))
              (delete-trailing-whitespace (point-min))
              (org-babel-trim (buffer-string)))
          ;;(search-backward "?-" nil t)
          ;;(kill-whole-line)
          (org-babel-eval-error-notify -1 (buffer-string))
          (org-babel-trim (buffer-string)))))))
(defun org-babel-pipelang-initiate-session (&optional session)
  "Return SESSION with a current inferior-process-buffer.

Initialize SESSION if it has not already been initialized."
  (unless  (equal "none" session)
    (let ((session (get-buffer-create (or session "*pipelang*"))))
      (unless (comint-check-proc session)
        (with-current-buffer session
          (kill-region (point-min) (point-max))
          (prolog-inferior-mode)
          (apply #'make-comint-in-buffer
                 "pipelang"
                 (current-buffer)
                 org-babel-pipelang-command
                 nil
                 (cons "-q" (pipelang-program-switches)))
          (add-hook 'comint-output-filter-functions
                    #'org-babel-pipelang--answer-correction nil t)
          (add-hook 'comint-output-filter-functions
                    #'org-babel-pipelang--exit-debug nil t)
          (add-hook 'comint-preoutput-filter-functions
                    #'ansi-color-apply nil t)
          (while (progn
                   (goto-char comint-last-input-end)
                   (not (save-excursion
                          (re-search-forward comint-prompt-regexp nil t))))
            (accept-process-output
             (get-buffer-process session)))))
      session)))
(defun org-babel-pipelang--session-load-clauses (session clauses)
  (with-current-buffer session
    (setq comint-prompt-regexp "^|: *"))
  (org-babel-comint-input-command session "consult(user).\n")
  (org-babel-comint-with-output (session "\n")
    (setq comint-prompt-regexp (pipelang-prompt-regexp))
    (dolist (line clauses)
      (insert line)
      (comint-send-input nil t)
      (accept-process-output
       (get-buffer-process session)))
    (comint-send-eof)))

(defun org-babel-pipelang-evaluate-external-process (goal body params)
  "Evaluate the GOAL given the BODY in an external Pipelang process.

If no GOAL is given, the GOAL is replaced with HALT.  This results in
running just the body through the Pipelang process."
  (let* ((tmp-file (org-babel-temp-file "pipelang-"))
         (pipelang-params (org-babel-pipelang-format-args params))
         (command (format "%s %s %s"
                          org-babel-pipelang-command
                          pipelang-params
                          tmp-file)))
    (message "All Params: %s" params)
    (with-temp-file tmp-file
      (insert (org-babel-chomp body)))
    (message "Going to execute: %s" command)
    (org-babel-pipelang-eval command "")))
(defun org-babel-pipelang-eval (cmd body)
  "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'.

NOTE: PIPELANG DOESN'T USE NORMAL EXIT CODES
see: https://www.mat.unical.it/aspcomp2013/files/aspoutput.txt
"
  (let ((err-buff (get-buffer-create " *Org-Babel Error*")) exit-code)
    (with-current-buffer err-buff (erase-buffer))
    (with-temp-buffer
      (insert body)
      (setq exit-code
            (org-babel--shell-command-on-region
             (point-min) (point-max) cmd err-buff))
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

(defun org-babel-pipelang--answer-correction (string)
  "If STRING is Pipelang's \"Correct to:\" prompt, send a refusal."
  (when (string-match-p "Correct to: \".*\"\\?" string)
    (comint-send-input nil t)))
(defun org-babel-pipelang--exit-debug (string)
  "If STRING indicates an exception, continue Pipelang execution in no debug mode."
  (when (string-match-p "\\(.\\|\n\\)*Exception.* \\? $" string)
    (comint-send-input nil t)))
(defun org-babel-pipelang-format-args (params)
  (mapconcat (lambda (x)
               (let ((sym (symbol-name (car x)))
                     (val (or (cdr x) ""))
                     )
                 (cond ((equal ":pipelang-args" sym)
                        (cdr x))
                       ((s-prefix? ":c-" sym)
                        (format "%s %s " (s-replace ":c-" "-" sym) val))
                       ((s-prefix? ":pipelang-" sym)
                        (format "%s %s " (s-replace ":pipelang-" "--" sym) val))
                       (""))
                 )
               )
             params "")
  )
(defun org-babel-pipelang--elisp-to-pl (value)
  "Convert the Emacs Lisp VALUE to equivalent Pipelang."
  (cond ((stringp value)
         (format "'%s'"
                 (replace-regexp-in-string
                  "'" "\\'" value)))
        ((listp value)
         (format "[%s]"
                 (mapconcat #'org-babel-pipelang--elisp-to-pl
                            value
                            ", ")))
        (t (prin1-to-string value))))
(defun org-babel-pipelang--variable-assignment (pair)
  "Return a string of a recorda/2 assertion of (cdr PAIR) under (car PAIR).

The Emacs Lisp value of the car of PAIR is used as the Key argument to
recorda/2 without modification.  The cdr of PAIR is converted to
equivalent Pipelang before being provided as the Term argument to
recorda/2."
  (format ":- recorda('%s', %s)."
          (car pair)
          (org-babel-pipelang--elisp-to-pl (cdr pair))))
(defun org-babel-variable-assignments:pipelang (params)
  "Return the babel variable assignments in PARAMS.

PARAMS is a quasi-alist of header args, which may contain
multiple entries for the key `:var'.  This function returns a
list of the cdr of all the `:var' entries."
  (let (vars)
    (dolist (param params vars)
      (when (eq :var (car param))
        (let ((var (org-babel-pipelang--variable-assignment (cdr param))))
          (setq vars (cons var vars)))))))
(defun org-babel-pipelang--parse-goal (goal)
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


(provide 'ob-pipelang)
;;; ob-pipelang.el ends here
