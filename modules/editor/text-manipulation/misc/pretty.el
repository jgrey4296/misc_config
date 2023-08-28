;; When you get to the right edge, it goes back to how it normally prints
(setq prettify-symbols-unprettify-at-point 'right-edge)

(cond
 ;; The emacs-mac build of Emacs appears to have built-in support for ligatures,
 ;; using the same composition-function-table method
 ;; https://bitbucket.org/mituharu/emacs-mac/src/26c8fd9920db9d34ae8f78bceaec714230824dac/lisp/term/mac-win.el?at=master#lines-345:805
 ;; so use that instead if this module is enabled.
 ((and IS-MAC (fboundp 'mac-auto-operator-composition-mode))
  (add-hook 'doom-init-ui-hook #'mac-auto-operator-composition-mode 'append))

 ;; Harfbuzz and Mac builds do not need font-specific ligature support
 ;; if they are above emacs-27.
 ((and (> emacs-major-version 27)
       (or (featurep 'ns)
           (string-match-p "HARFBUZZ" system-configuration-features))
       (featurep 'composite))  ; Emacs loads `composite' at startup
  (defvar +ligature--composition-table (make-char-table nil))
  (add-hook! 'doom-init-ui-hook :append
    (defun +ligature-init-composition-table-h ()
      (dolist (char-regexp +ligatures-composition-alist)
        (set-char-table-range
         +ligature--composition-table
         (car char-regexp) `([,(cdr char-regexp) 0 font-shape-gstring])))
      (set-char-table-parent +ligature--composition-table composition-function-table))))

 ;; Fallback ligature support for certain, patched fonts. Install them with
 ;; `+ligatures/install-patched-font'
 ((defmacro +ligatures--def-font (id font-plist &rest alist)
    (declare (indent 2))
    (let ((alist-var (intern (format "+ligatures-%s-font-alist" id)))
          (setup-fn  (intern (format "+ligatures-init-%s-font-h" id))))
      `(progn
         (setf (alist-get ',id +ligatures--font-alist) (list ,@font-plist))
         (defvar ,alist-var ',alist ,(format "Name of the %s ligature font." id))
         (defun ,setup-fn (&rest _)
           (cl-destructuring-bind (name &key _url files range)
               (or (alist-get ',id +ligatures--font-alist)
                   (error "No ligature font called %s" ',id))
             (when range
               (set-fontset-font t range name nil 'prepend))
             (setq-default prettify-symbols-alist
                           (append (default-value 'prettify-symbols-alist)
                                   (mapcar #'+ligatures--correct-symbol-bounds ,alist-var)))))
         (add-hook '+ligatures--init-font-hook #',setup-fn))))

  (defvar +ligatures--font-alist ())

  (local-load! "+fira"))
 )

(defun +ligatures--correct-symbol-bounds (ligature-alist)
  "Prepend non-breaking spaces to a ligature.

This way `compose-region' (called by `prettify-symbols-mode') will use the
correct width of the symbols instead of the width measured by `char-width'."
  (let ((len (length (car ligature-alist)))
        (acc (list   (cdr ligature-alist))))
    (while (> len 1)
      (setq acc (cons #X00a0 (cons '(Br . Bl) acc))
            len (1- len)))
    (cons (car ligature-alist) acc)))
