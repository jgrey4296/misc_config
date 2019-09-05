;;; ob-ccalc.el --- org-babel functions for ccalc evaluation.

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

;; Org-babel support for prolog.
;;
;; To activate ob-prolog add the following to your init.el file:
;;
;;  (add-to-list 'load-path "/path/to/ob-prolog-dir")
;;  (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((prolog . t)))
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


(add-to-list 'org-babel-tangle-lang-exts '("ccalc" . "pl"))

(defvar org-babel-ccalc-location "~/github/otherlibs/ccalc/ccalc.pl")
(defvar org-babel-ccalc-command  "swipl6"
  "Name of the ccalc executable command.")
(defconst org-babel-header-args:ccalc
  '((:goal . :any))
  "Ccalc-specific header arguments.")
(defvar org-babel-default-header-args:ccalc
  `((:goal . nil)))

(defun org-babel-execute:ccalc (body params)
  "Execute the Ccalc in BODY according to the block's header PARAMS.

This function is called by `org-babel-execute-src-block.'"
  (message "executing Ccalc source code block")
  (let* ((result-params (cdr (assq :result-params params)))
         (session (format "*CCalc: %s*" (cdr (assq :session params))))
         (treat-as-file (assq :as-file params))
         (goal (org-babel-ccalc--parse-goal
                (cdr (assq :goal params))))
         (vars (org-babel-variable-assignments:ccalc params))
         (full-body (org-babel-expand-body:generic body params vars))
         (results (if (string= "none" session)
                      (org-babel-ccalc-evaluate-external-process
                       goal full-body)
                    (org-babel-ccalc-evaluate-session
                     session goal full-body treat-as-file))))
    (unless (string= "" results)
      (org-babel-reassemble-table
       (org-babel-result-cond result-params
         results
         (let ((tmp (org-babel-temp-file "ccalc-results-")))
           (with-temp-file tmp (insert results))
           (org-babel-import-elisp-from-file tmp)))
       (org-babel-pick-name (cdr (assq :colname-names params))
                            (cdr (assq :colnames params)))
       (org-babel-pick-name (cdr (assq :rowname-names params))
                            (cdr (assq :rownames params)))))))

(defun org-babel-ccalc-evaluate-session (session goal body as-file)
  "In SESSION, evaluate GOAL given the BODY of the Ccalc block.

Create SESSION if it does not already exist."
  (let* ((session (org-babel-ccalc-initiate-session session))
         (split-body (split-string (org-babel-trim body) "\n")))
    (if as-file
        (let* ((tmp-file (org-babel-temp-file "ccalc-" ".pl")))
          (message "Treating as file")
          (with-temp-file tmp-file (insert (org-babel-chomp body)))
          (setq split-body `(,(format "loadf '%s'." tmp-file)))
          ))
    (with-temp-buffer
      (apply #'insert (org-babel-ccalc--session-load-clauses session split-body))
      (if (save-excursion (search-backward "ERROR: " nil t))
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

(defun org-babel-ccalc-initiate-session (&optional session)
  "Return SESSION with a current inferior-process-buffer.
Initialize SESSION if it has not already been initialized."
  (unless  (equal "none" session)
    (let ((session (get-buffer-create (or session "*ccalc*"))))
      (unless (comint-check-proc session)
        (with-current-buffer session
          (kill-region (point-min) (point-max))
          (prolog-inferior-mode)
          (apply #'make-comint-in-buffer
                 "ccalc"
                 (current-buffer)
                 org-babel-ccalc-command
                 nil)
          (add-hook 'comint-output-filter-functions
                    #'org-babel-ccalc--answer-correction nil t)
          (add-hook 'comint-output-filter-functions
                    #'org-babel-ccalc--exit-debug nil t)
          (add-hook 'comint-preoutput-filter-functions
                    #'ansi-color-apply nil t)
          (while (progn
                   (goto-char comint-last-input-end)
                   (not (save-excursion
                          (re-search-forward comint-prompt-regexp nil t))))
            (accept-process-output
             (get-buffer-process session)))
          )
        (org-babel-ccalc--session-load-clauses session `(,(format "['%s']." org-babel-ccalc-location)))
        )
      session)
    ))

(defun org-babel-ccalc--session-load-clauses (session clauses)
  (with-current-buffer session
    (setq comint-prompt-regexp "^|: *"))
  (org-babel-comint-with-output (session "\n")
    (setq comint-prompt-regexp (prolog-prompt-regexp))
    (dolist (line clauses)
      (insert line)
      (comint-send-input nil t)
      (accept-process-output
       (get-buffer-process session)))
    (comint-send-input)))

(defun org-babel-ccalc-evaluate-external-process (goal body)
  "Evaluate the GOAL given the BODY in an external Ccalc process.

If no GOAL is given, the GOAL is replaced with HALT.  This results in
running just the body through the Ccalc process."
  (message "RUNNING EXTERNAL PROCESS")
  (let* ((tmp-file (org-babel-temp-file "ccalc-"))
         (command (format "%s --quiet -l %s -g \"%s\" -t 'halt'"
                          org-babel-ccalc-command
                          tmp-file
                          (replace-regexp-in-string
                           "\"" "\\\"" (or goal "halt")))))
    (with-temp-file tmp-file
      (insert (org-babel-chomp body)))
    (or (org-babel-eval command "") "")))

(defun org-babel-ccalc--answer-correction (string)
  "If STRING is Ccalc's \"Correct to:\" prompt, send a refusal."
  (when (string-match-p "Correct to: \".*\"\\?" string)
    (message "Answer Correction")
    (comint-send-input nil t)))
(defun org-babel-ccalc--exit-debug (string)
  "If STRING indicates an exception, continue Ccalc execution in no debug mode."
  (when (string-match-p "\\(.\\|\n\\)*Exception.* \\? $" string)
    (message "Exit Debug")
    (comint-send-input nil t)))
(defun org-babel-ccalc--elisp-to-pl (value)
  "Convert the Emacs Lisp VALUE to equivalent Ccalc."
  (cond ((stringp value)
         (format "'%s'"
                 (replace-regexp-in-string
                  "'" "\\'" value)))
        ((listp value)
         (format "[%s]"
                 (mapconcat #'org-babel-ccalc--elisp-to-pl
                            value
                            ", ")))
        (t (prin1-to-string value))))
(defun org-babel-ccalc--variable-assignment (pair)
  "Return a string of a recorda/2 assertion of (cdr PAIR) under (car PAIR).

The Emacs Lisp value of the car of PAIR is used as the Key argument to
recorda/2 without modification.  The cdr of PAIR is converted to
equivalent Ccalc before being provided as the Term argument to
recorda/2."
  (format ":- recorda('%s', %s)."
          (car pair)
          (org-babel-ccalc--elisp-to-pl (cdr pair))))
(defun org-babel-variable-assignments:ccalc (params)
  "Return the babel variable assignments in PARAMS.

PARAMS is a quasi-alist of header args, which may contain
multiple entries for the key `:var'.  This function returns a
list of the cdr of all the `:var' entries."
  (let (vars)
    (dolist (param params vars)
      (when (eq :var (car param))
        (let ((var (org-babel-ccalc--variable-assignment (cdr param))))
          (setq vars (cons var vars)))))))
(defun org-babel-ccalc--parse-goal (goal)
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


(provide 'ob-ccalc)
;;; ob-ccalc.el ends here
