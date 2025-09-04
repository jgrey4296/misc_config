;;; utils.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(defvar jg-eval--compile-keys '(:read :recursive :interactive))
(defconst jg-eval--compile-text-syms '(blddir bldenv extra cmd))

;;;###autoload
(defun +jg-eval--annotate-cmds (cmds act-fn &optional int-fn)
  " set the `cmd` text property for each string to the result of (act-fn str) "
  (cl-loop for cmd in cmds
           collect (let ((cmd-str cmd)
                         (cmd-act (funcall act-fn cmd))
                         (extras (list (when (and int-fn (funcall int-fn cmd)) :interactive)))
                         )
                     ;; The cmd text prop is what is actually run, the text itself is what is shown
                     (set-text-properties 0 (length cmd-str) `(cmd ,cmd-act extras ,extras) cmd-str)
                     cmd-str))
  )

;;;###autoload
(defun +jg-eval--pair-cmds (&rest cmds)
  " for each pair, set the `cmd` text-property of car to cdr
can also add key'd additional properties
"
  (cl-loop for val-lst in cmds
           collect
           (let ((usr-str (car val-lst))
                 (cmd (cadr val-lst))
                 (extra (cddr val-lst))
                 )
             (set-text-properties 0 (length usr-str) `(cmd ,cmd extra ,extra) usr-str)
             usr-str
             )
           )
  )

;;;###autoload
(defun +jg-eval--run-compile (cmd)
  "call cmd, but if it's got the text propery 'cmd use that instead,

:interactive -> call the reuslt in a comint.
:read        -> read a string before formatting the cmd
:recursive   -> TODO

 "
  (let* ((blddir (get-text-property 0 'blddir cmd))
         (bldenv (get-text-property 0 'bldenv cmd))
         (extras (get-text-property 0 'extra cmd))
         (cmd-str (or (get-text-property 0 'cmd cmd) cmd))
         (interactive? (memq :interactive extras))
         (interactive-mode (if (functionp (car-safe (cdr-safe interactive?)))
                               (cadr interactive?)
                             t))
         )
    (if (or (get-text-property 0 'recursive cmd) (memq :recursive extras))
        (counsel-compile blddir)
      (let ((default-directory (or blddir counsel-compile--current-build-dir default-directory))
            (compilation-environment bldenv))
        ;; No need to specify `:history' because of this hook.
        (add-hook 'compilation-start-hook #'counsel-compile--update-history)
        (unwind-protect
            ;; TODO change this to pairs of (:read {prompt})
            (let ((compile-str (if (memq :read extras)
                                   (concat cmd-str " " (read-string (format "%s _: " cmd-str)))
                                 cmd-str))
                  )
              (compile compile-str
                       (when interactive? interactive-mode))
              (remove-hook 'compilation-start-hook #'counsel-compile--update-history)
              )
          )
        )
      )
    )
  )

;;;###autoload
(defun +jg-eval--projectile-cmd-list  (fn &rest rst)
  " Use an ivy to get the command "
  (let* ((compilation-read-command nil)
        (root (projectile-project-root))
        (cmd-cache (f-join root jg-projects-cmd-cache-name))
        (candidates (if (and root (f-exists? cmd-cache))
                        (with-temp-buffer
                          (insert-file-contents cmd-cache)
                          (sort (s-split "\n" (buffer-string) t) 'string-lessp))))
        (ivy-val (ivy-read (plist-get rst :prompt-prefix)
                           candidates)))
    (if (not (-contains? candidates ivy-val))
        (append-to-file (format "%s\n" ivy-val) nil cmd-cache)
        )
    (apply fn (cons ivy-val (cdr rst)))
    )
  )

;;;###autoload
(defun +jg-eval--time-compile-cmd-retrieval (&optional dir)
  " For Debugging how different local build cmd constructors perform"
  (let (cands times)
    (dolist (cmds counsel-compile-local-builds)
      (when (functionp cmds)
        (push (list (symbol-name cmds)
                    (benchmark-elapse
                      (setq cmds (funcall cmds dir)))) times))
      (when cmds
        (push (if (listp cmds) cmds (list cmds)) cands)))
    (cl-loop for pair in times
             do
             (message "%-10s : %s" (cadr pair) (car pair))
             )
    (apply #'append (nreverse cands)))
  )

;;;###autoload
(defun +jg-eval--compile-root-fallback ()
  default-directory
  )



;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    November 15, 2024
;; Modified:   November 15, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; utils.el ends here
