;;; minor-modes.el -*- lexical-binding: t; -*-

(defun +emacs-lisp--in-package-buffer-p ()
  (let* ((file-path (buffer-file-name (buffer-base-buffer)))
         (file-base (if file-path (file-name-base file-path))))
    (and (derived-mode-p 'emacs-lisp-mode)
         (or (null file-base)
             (locate-file file-base (custom-theme--load-path) '(".elc" ".el"))
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-max))
                 (when (re-search-backward "^ *\\((provide\\)\\(?:-theme\\)? +'"
                                           (max (point-min) (- (point-max) 512))
                                           t)
                   (goto-char (match-beginning 1))
                   (ignore-errors
                     (and (stringp file-base)
                          (equal (symbol-name (doom-unquote (nth 1 (read (current-buffer)))))
                                 file-base)))))))
         (not (locate-dominating-file default-directory ".doommodule")))))

;;;###autoload
(define-minor-mode +emacs-lisp-non-package-mode
  "Reduce flycheck verbosity where it is appropriate.

Essentially, this means in any elisp file that either:
- Is not a theme in `custom-theme-load-path',
- Lacks a `provide' statement,
- Lives in a project with a .doommodule file,
- Is a dotfile (like .dir-locals.el or .doomrc).

This generally applies to your private config (`doom-user-dir') or Doom's source
\(`doom-emacs-dir')."
  :since "3.0.0"
  (unless (and (bound-and-true-p flycheck-mode)
               (not (+emacs-lisp--in-package-buffer-p)))
    (setq +emacs-lisp-non-package-mode nil))
  (when (derived-mode-p 'emacs-lisp-mode)
    (add-hook 'after-save-hook #'+emacs-lisp-non-package-mode nil t))
  (if (not +emacs-lisp-non-package-mode)
      (when (get 'flycheck-disabled-checkers 'initial-value)
        (setq-local flycheck-disabled-checkers (get 'flycheck-disabled-checkers 'initial-value))
        (kill-local-variable 'flycheck-emacs-lisp-check-form))
    (with-memoization (get 'flycheck-disabled-checkers 'initial-value)
      flycheck-disabled-checkers)
    (setq-local flycheck-emacs-lisp-check-form
                (prin1-to-string
                 `(progn
                    (setq doom-modules ',doom-modules
                          doom-disabled-packages ',doom-disabled-packages
                          byte-compile-warnings ',+emacs-lisp-linter-warnings)
                    (condition-case e
                        (progn
                          (require 'doom)
                          (require 'doom-cli)
                          (require 'doom-start))
                      (error
                       (princ
                        (format "%s:%d:%d:Error:Failed to load Doom: %s\n"
                                (or ,(ignore-errors
                                       (file-name-nondirectory
                                        (buffer-file-name (buffer-base-buffer))))
                                    (car command-line-args-left))
                                0 0 (error-message-string e)))))
                    ,(read (default-toplevel-value 'flycheck-emacs-lisp-check-form))))
                flycheck-disabled-checkers (cons 'emacs-lisp-checkdoc
                                                 flycheck-disabled-checkers))))
