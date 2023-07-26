;;; lookup.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +emacs-lisp-lookup-definition (_thing)
  "Lookup definition of THING."
  (if-let (module (+emacs-lisp--module-at-point))
      (doom/help-modules (car module) (cadr module) 'visit-dir)
    (call-interactively #'elisp-def)))

;;;###autoload
(defun +emacs-lisp-lookup-documentation (thing)
  "Lookup THING with `helpful-variable' if it's a variable, `helpful-callable'
if it's callable, `apropos' otherwise."
  (cond ((when-let (module (+emacs-lisp--module-at-point))
           (doom/help-modules (car module) (cadr module))
           (when (eq major-mode 'org-mode)
             (with-demoted-errors "%s"
               (re-search-forward
                (if (caddr module)
                    "\\* Module flags$"
                  "\\* Description$"))
               (when (caddr module)
                 (re-search-forward (format "=\\%s=" (caddr module))
                                    nil t))
               (when (invisible-p (point))
                 (org-show-hidden-entry))))
           'deferred))
        (thing (helpful-symbol (intern thing)))
        ((call-interactively #'helpful-at-point))))



;;;###autoload
(defun +racket-lookup-documentation (thing)
  "A `+lookup/documentation' handler for `racket-mode' and `racket-xp-mode'."
  (let ((buf (if racket-xp-mode
                 (racket-xp-describe thing)
               (racket-repl-describe thing))))
    (when buf
      (pop-to-buffer buf)
      t)))

;;;###autoload
(defun +racket-lookup-definition (_thing)
  "A `+lookup/definition' handler for `racket-mode' and `racket-xp-mode'."
  (call-interactively
   (if racket-xp-mode
       #'racket-xp-visit-definition
     #'racket-repl-visit-definition)))
