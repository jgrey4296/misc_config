;;; ui/ligatures/autoload/ligatures.el -*- lexical-binding: t; -*-


;;;###autodef
(defun set-ligatures! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.

  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in
`+ligatures-extra-symbols', and whose values are strings representing the text
to be replaced with that symbol. If the car of PLIST is nil, then unset any
pretty symbols previously defined for MODES.

This function accepts one special property:

  :alist ALIST
    Appends ALIST to `prettify-symbols-alist' literally, without mapping text to
    `+ligatures-extra-symbols'.

For example, the rule for emacs-lisp-mode is very simple:

  (set-ligatures! 'emacs-lisp-mode
    :lambda \"lambda\")

This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
assicated with :lambda in `+ligatures-extra-symbols'.

Pretty symbols can be unset for emacs-lisp-mode with:

  (set-ligatures! 'emacs-lisp-mode nil)"
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (ensure-list modes))
        (delq! mode +ligatures-extra-alist 'assq))
    (let (results)
      (while plist
        (let ((key (pop plist)))
          (if (eq key :alist)
              (prependq! results (pop plist))
            (when-let (char (plist-get +ligatures-extra-symbols key))
              (push (cons (pop plist) char) results)))))
      (dolist (mode (ensure-list modes))
        (setf (alist-get mode +ligatures-extra-alist)
              (if-let (old-results (alist-get mode +ligatures-extra-alist))
                  (dolist (cell results old-results)
                    (setf (alist-get (car cell) old-results) (cdr cell)))
                results))))))
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

(defun +ligatures--enable-p (modes)
  "Return t if ligatures should be enabled in this buffer depending on MODES."
  (unless (eq major-mode 'fundamental-mode)
    (or (eq modes t)
        (if (eq (car modes) 'not)
            (not (apply #'derived-mode-p (cdr modes)))
          (apply #'derived-mode-p modes)))))

;;;###autoload
(defun +ligatures-init-buffer-h ()
  "Set up ligatures for the current buffer.

Extra ligatures are mode-specific substituions, defined in
`+ligatures-extra-symbols', assigned with `set-ligatures!', and made possible
with `prettify-symbols-mode'. This variable controls where these are enabled.
See `+ligatures-extras-in-modes' to control what major modes this function can
and cannot run in."
  (when after-init-time
    (let ((in-mode-p
           (+ligatures--enable-p +ligatures-in-modes))
          (in-mode-extras-p
           (and (modulep! +extra)
                (+ligatures--enable-p +ligatures-extras-in-modes))))
      (when in-mode-p
        (if (boundp '+ligature--composition-table)
            (setq-local composition-function-table +ligature--composition-table)
          (run-hooks '+ligatures--init-font-hook)
          (setq +ligatures--init-font-hook nil)))
      (when in-mode-extras-p
        (prependq! prettify-symbols-alist
                   (alist-get major-mode +ligatures-extra-alist)))
      (when (and (or in-mode-p in-mode-extras-p)
                 prettify-symbols-alist)
        (when prettify-symbols-mode
          (prettify-symbols-mode -1))
        (prettify-symbols-mode +1)))))
