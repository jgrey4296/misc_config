;;; lang/emacs-lisp/autoload.el -*- lexical-binding: t; -*-

;;
;;; Library
;; DEPRECATED Remove when 28 support is dropped.
(unless (fboundp 'lisp--local-defform-body-p)
  (fset 'lisp--local-defform-body-p #'ignore))

;;;###autoload
(defun +emacs-lisp-outline-level ()
  "Return outline level for comment at point.
Intended to replace `lisp-outline-level'."
  (- (match-end 1) (match-beginning 1)))

;;;###autoload
(defun +emacs-lisp--module-at-point ()
  "Return (CATEGORY MODULE FLAG) at point inside a `doom!' block."
  (let ((origin (point))
        (syntax (syntax-ppss)))
    (when (and (> (ppss-depth syntax) 0) (not (ppss-string-terminator syntax)))
      (save-excursion
        (let ((parens (ppss-open-parens syntax))
              (doom-depth 1))
          (while (and parens (progn (goto-char (car parens))
                                    (not (looking-at "(doom!\\_>"))))
            (setq parens (cdr parens)
                  doom-depth (1+ doom-depth)))
          (when parens ;; Are we inside a `doom!' block?
            (goto-char origin)
            (let* ((doom-start (car parens))
                   (bare-symbol
                    (if (ppss-comment-depth syntax)
                        (= (save-excursion (beginning-of-thing 'list)) doom-start)
                      (null (cdr parens))))
                   (sexp-start (if bare-symbol
                                   (beginning-of-thing 'symbol)
                                 (or (cadr parens) (beginning-of-thing 'list))))
                   (match-start nil))
              (goto-char sexp-start)
              (while (and (not match-start)
                          (re-search-backward
                           "\\_<:\\(?:\\sw\\|\\s_\\)+\\_>" ;; Find a keyword.
                           doom-start 'noerror))
                (unless (looking-back "(")
                  (let ((kw-syntax (syntax-ppss)))
                    (when (and (= (ppss-depth kw-syntax) doom-depth)
                               (not (ppss-string-terminator kw-syntax))
                               (not (ppss-comment-depth kw-syntax)))
                      (setq match-start (point))))))
              (when match-start
                (let (category module flag)
                  ;; `point' is already at `match-start'.
                  (setq category (symbol-at-point))
                  (goto-char origin)
                  (if bare-symbol
                      (setq module (symbol-at-point))
                    (let ((symbol (symbol-at-point))
                          (head (car (list-at-point))))
                      (if (and (symbolp head) (not (keywordp head))
                               (not (eq head symbol)))
                          (setq module head
                                flag symbol)
                        (setq module symbol))))
                  (list category module flag))))))))))

;;;###autoload
(defun +emacs-lisp-indent-function (indent-point state)
  "A replacement for `lisp-indent-function'.

Indents plists more sensibly. Adapted from
https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned"
  (let ((normal-indent (current-column))
        (orig-point (point))
        ;; TODO Refactor `target' usage (ew!)
        target)
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond ((and (elt state 2)
                (or (eq (char-after) ?:)
                    (not (looking-at-p "\\sw\\|\\s_"))))
           (if (lisp--local-defform-body-p state)
               (lisp-indent-defform state indent-point)
             (unless (> (save-excursion (forward-line 1) (point))
                        calculate-lisp-indent-last-sexp)
               (goto-char calculate-lisp-indent-last-sexp)
               (beginning-of-line)
               (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
             (backward-prefix-chars)
             (current-column)))
          ((and (save-excursion
                  (goto-char indent-point)
                  (skip-syntax-forward " ")
                  (not (eq (char-after) ?:)))
                (save-excursion
                  (goto-char orig-point)
                  (and (eq (char-after) ?:)
                       (eq (char-before) ?\()
                       (setq target (current-column)))))
           (save-excursion
             (move-to-column target t)
             target))
          ((let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
                  (method (or (function-get (intern-soft function) 'lisp-indent-function)
                              (get (intern-soft function) 'lisp-indent-hook))))
             (cond ((or (eq method 'defun)
                        (and (null method)
                             (> (length function) 3)
                             (string-match-p "\\`def" function)))
                    (lisp-indent-defform state indent-point))
                   ((integerp method)
                    (lisp-indent-specform method state indent-point normal-indent))
                   (method
                    (funcall method indent-point state))))))))

;;;###autoload
(defun +emacs-lisp/buttercup-run-file ()
  "Run all buttercup tests in the focused buffer."
  (interactive)
  (let ((load-path
         (append (list (doom-path (dir!) "..")
                       (or (doom-project-root)
                           default-directory))
                 load-path))
        (buttercup-suites nil))
    (save-selected-window
      (eval-buffer)
      (buttercup-run))
    (message "File executed successfully")))

;;;###autoload
(defun +emacs-lisp/buttercup-run-project ()
  "Run all buttercup tests in the project."
  (interactive)
  (let* ((default-directory (doom-project-root))
         (load-path (append (list (doom-path "test")
                                  default-directory)
                            load-path))
         (buttercup-suites nil))
    (buttercup-run-discover)))

;;;###autoload
(defun +emacs-lisp/edebug-instrument-defun-on ()
  "Toggle on instrumentalisation for the function under `defun'."
  (interactive)
  (eval-defun 'edebugit))

;;;###autoload
(defun +emacs-lisp/edebug-instrument-defun-off ()
  "Toggle off instrumentalisation for the function under `defun'."
  (interactive)
  (eval-defun nil))

;;; autoload.el ends here
