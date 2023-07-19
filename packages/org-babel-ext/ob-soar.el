;;; soar-mode/ob-soar.el -*- lexical-binding: t; -*-
;; ob-template,
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

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(defvar ob-soar-ext "soar")
;; TODO
(defvar ob-soar-cmd-str "")
;; Map Languages to extensions
(add-to-list 'org-babel-tangle-lang-exts '(".soar" . "soar"))
;; Default Headers, as a plist:
(defconst org-babel-header-args:soar
  '()
  "soar-specific header arguments.")
(defvar org-babel-default-header-args:soar
  `())

(defvar org-babel-soar-command  "soar"
  "Name of the soar executable command.")

(defun org-babel-execute:soar (body params)
  "Execute the soar in BODY according to the block's header PARAMS.

This function is called by org-babel-execute-src-block.'"
  (message "executing soar source code block")
  (let* ((result-params (cdr (assq :result-params params)))
         (session (cdr (assq :session params)))
         (treat-as-file (assq :as-file params))
         (vars (org-babel-variable-assignments:soar params))
         ;; Write your own expand body if you need to wrap code:
         (full-body (org-babel-expand-body:generic body params vars))
         ;; Get the results of execution, in a session or a single process:
         (results (if (string= "none" session)
                      (org-babel-soar-evaluate-external-process
                       full-body)
                    (org-babel-soar-evaluate-session
                     session goal full-body treat-as-file))))
    ;; Return the results, post-process here if necessary
    results))

(defun org-babel-soar-evaluate-session (session body as-file)
  "In SESSION, send the BODY of the soar block.

Create SESSION if it does not already exist."
  (message "Evaluating soar session")
  (let* ((session (org-babel-soar-initiate-session session))
         (split-body (split-string (org-babel-trim body) "\n")))
    (if as-file
        (let* ((tmp-file (org-babel-temp-file "soar-" ob-soar-ext)))
          (message "Treating as file")
          (with-temp-file tmp-file (insert (org-babel-chomp body)))
          ;; TODO specify a load statement for your process here
          (setq split-body nil)
          ))
    (with-temp-buffer
      (apply #'insert (org-babel-soar--session-load-clauses session split-body))
      ;; Handle errors if your comint complains:
      (if (save-excursion (search-backward "ERROR: " nil t))
          (org-babel-eval-error-notify -1 (buffer-string))
        ;; Otherwise:
        (let ((delete-trailing-lines t))
          (delete-trailing-whitespace (point-min))
          (org-babel-trim (buffer-string)))))))
(defun org-babel-soar-initiate-session (&optional session)
  "Return SESSION with a current inferior-process-buffer.
Initialize SESSION if it has not already been initialized."
  (unless  (equal "none" session)
    (message "Initialising soar session")
    (let ((session (get-buffer-create (or session "*soar*"))))
      (unless (comint-check-proc session)
        (with-current-buffer session
          (kill-region (point-min) (point-max))
          ;; TODO set the comint buffer mode here
          (fundamental-mode)
          (apply #'make-comint-in-buffer
                 "soar"
                 (current-buffer)
                 org-babel-soar-command
                 nil)
          ;; Setup comint handling hooks here
          (add-hook 'comint-output-filter-functions
                    #'org-babel-soar--answer-correction nil t)
          (add-hook 'comint-output-filter-functions
                    #'org-babel-soar--exit-debug nil t)
          (add-hook 'comint-preoutput-filter-functions
                    #'ansi-color-apply nil t)
          (while (progn
                   (goto-char comint-last-input-end)
                   (not (save-excursion
                          (re-search-forward comint-prompt-regexp nil t))))
            (accept-process-output
             (get-buffer-process session)))
          )
        (org-babel-soar--session-load-clauses session `(,(format "['%s']." org-babel-soar-location)))
        )
      session)
    ))
(defun org-babel-soar--session-load-clauses (session clauses)
  " insert a list of clauses into your comint session "
  (with-current-buffer session
    (setq comint-prompt-regexp "^|: *"))
  (org-babel-comint-with-output (session "\n")
    (dolist (line clauses)
      (insert line)
      (comint-send-input nil t)
      (accept-process-output
       (get-buffer-process session)))
    (comint-send-input)))

(defun org-babel-soar-evaluate-external-process (body)
  "Eval the BODY in an external soar process. "
  (message "External process soar")
  (let* ((soar-tmp-file (org-babel-temp-file "soar-" ob-soar-ext))
         (soar-params (org-babel-soar-format-args params))
         ;; TODO format the command here
         (command (format ob-soar-cmd-str
                          org-babel-soar-command
                          (expand-file-name org-babel-soar-location)
                          ))
         )
    ;; (message "All Params: %s" params)
    ;; (message "Body: %s" body)
    ;; (message "soar File: %s\n Prolog File: %s" soar-tmp-file prolog-tmp-file)
    (with-temp-file soar-tmp-file
      ;;Insert the soar code into a temp file
      (insert (org-babel-chomp body))
      )
    ;; (message "Going to execute:\n%s" command)
    (org-babel-soar-eval command body)
    )
  )
(defun org-babel-soar-eval (cmd body)
  "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with org-babel-eval-error-notify'.
"
  (let ((err-buff (get-buffer-create "*Org-Babel Error Output*"))
        exit-code)
    (with-current-buffer err-buff (erase-buffer))
    (with-temp-buffer
      (insert body)
      (setq exit-code
            (org-babel--shell-command-on-region cmd err-buff))
      ;; exit code handling:
      (message "soar EXIT CODE: %s" exit-code)
      (cond ((not (numberp exit-code))
             (with-current-buffer err-buff
               (org-babel-eval-error-notify exit-code (buffer-string)))
             nil)
            (t
             (buffer-string))))))


(defun org-babel-soar--answer-correction (string)
  "If STRING is soar's "Correct to:" prompt, send a refusal."
  (when (string-match-p "Correct to: ".*"\?" string)
    (message "Answer Correction")
    (comint-send-input nil t)))
(defun org-babel-soar--exit-debug (string)
  "If STRING indicates an exception, continue soar execution in no debug mode."
  (when (string-match-p "\(.\|\n\)*Exception.* \? $" string)
    (message "Exit Debug")
    (comint-send-input nil t)))
(defun org-babel-soar--elisp-to-soar (value)
  "Convert the Emacs Lisp VALUE to equivalent soar."
  (cond ((stringp value)
         (format "'%s'"
                 (replace-regexp-in-string
                  "'" "\'" value)))
        ((listp value)
         (format "[%s]"
                 (mapconcat #'org-babel-soar--elisp-to-soar
                            value
                            ", ")))
        (t (prin1-to-string value))))

(defun org-babel-soar-format-args (params)
  " Adapt the parameters passed in into soar's CLI arguments "
  (mapconcat (lambda (x)
               (let ((sym (symbol-name (car x)))
                     (val (or (cdr x) "")))
                 (format "%s %s" sym val)))
             params "")
  )

(provide 'ob-soar)
;;; ob-soar.el ends here
