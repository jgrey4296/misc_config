;;; visuals.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +emacs-lisp-truncate-pin ()
  "Truncates long SHA1 hashes in `package!' :pin's."
  (save-excursion
    (goto-char (match-beginning 0))
    (and (stringp (plist-get (sexp-at-point) :pin))
         (search-forward ":pin" nil t)
         (let ((start (re-search-forward "\"[^\"\n]\\{12\\}" nil t))
               (finish (and (re-search-forward "\"" (line-end-position) t)
                            (match-beginning 0))))
           (when (and start finish)
             (put-text-property start finish 'display "...")))))
  nil)

(defvar +emacs-lisp--face nil)

;;;###autoload
(defun +emacs-lisp-highlight-vars-and-faces (end)
  "Match defined variables and functions.

Functions are differentiated into special forms, built-in functions and
library/userland functions"
  (catch 'matcher
    (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
      (let ((ppss (save-excursion (syntax-ppss))))
        (cond ((nth 3 ppss)  ; strings
               (search-forward "\"" end t))
              ((nth 4 ppss)  ; comments
               (forward-line +1))
              ((let ((symbol (intern-soft (match-string-no-properties 0))))
                 (and (cond ((null symbol) nil)
                            ((eq symbol t) nil)
                            ((keywordp symbol) nil)
                            ((special-variable-p symbol)
                             (setq +emacs-lisp--face 'font-lock-variable-name-face))
                            ((and (fboundp symbol)
                                  (eq (char-before (match-beginning 0)) ?\()
                                  (not (memq (char-before (1- (match-beginning 0)))
                                             (list ?\' ?\`))))
                             (let ((unaliased (indirect-function symbol)))
                               (unless (or (macrop unaliased)
                                           (special-form-p unaliased))
                                 (let (unadvised)
                                   (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                   (setq unaliased (indirect-function unadvised)))))
                                   unaliased)
                                 (setq +emacs-lisp--face
                                       (if (subrp unaliased)
                                           'font-lock-constant-face
                                         'font-lock-function-name-face))))))
                      (throw 'matcher t)))))))
    nil))

;; HACK: Quite a few functions here are called often, and so are especially
;;   performance sensitive, so we compile this file on-demand, at least, until
;;   Doom adds a formal compile step to 'doom sync'.
(doom-compile-functions #'+emacs-lisp-highlight-vars-and-faces
                        #'+emacs-lisp-truncate-pin
                        #'+emacs-lisp--calculate-lisp-indent-a)
