;;; config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(load! "utils/+state-hl-lines")
(load! "utils/+faces")
(load! "utils/+narrowing")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(add-hook! 'doom-init-ui-hook  'rainbow-delimiters-mode)
(add-hook! 'doom-init-ui-hook (defun +jg-ui-load-advice () (load! "utils/+advice")))

(use-package! hl-line
  :defer t
  :init
  (global-hl-line-mode)
  )
(use-package! hi-lock
  :defer t
  :init
  (global-hi-lock-mode)
  :config
  (setq hi-lock-auto-select-face t)
  )
(use-package! auto-highlight-symbol
  :commands auto-highlight-symbol-mode
  :init
  (defvar auto-highlight-symbol-mode nil)
  )
(use-package! whitespace
  :commands whitespace-mode
  :init
  (defvar whitespace-mode nil)
  )
(use-package! centered-cursor-mode
  :commands centered-cursor-mode
  :init
  (defvar centered-cursor-mode nil)
  )
(use-package! window-ring-minor-mode
  :commands (window-ring-setup-columns window-ring-minor-mode window-ring-setup-columns-command)
  )
(use-package! palette-mode
  :mode ("\\.palette" . palette-mode)
  :commands palette-mode
  )
(use-package! evil-visual-mark-mode :defer t)

(use-package! prettify-symbosl
  :config
  ;; When you get to the right edge, it goes back to how it normally prints
  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (add-hook! 'doom-init-ui-hook :append
    (defun +ligatures-init-h ()
      (add-hook 'after-change-major-mode-hook #'+ligatures-init-buffer-h)))

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

    (load! "+fira"))
  )
