;;; +derived-modes.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;-- scons

;;;###autoload
(define-derived-mode scons-mode python-mode "scons"
  ""
  (interactive)
  (setq-local major-mode 'scons-mode)
  (setq-local mode-name  "scons")
  (run-mode-hooks)
  )

;;-- end scons

;;-- doit/doot

;;;###autoload
(define-derived-mode doot-mode python-mode "doot"
  ""
  (interactive)
  (setq-local major-mode 'doot-mode)
  (setq-local mode-name  "doot")
  (run-mode-hooks)
  )

;;;###autoload
(defun doot-open-toml ()
  (interactive)
  (when (and (s-matches? "dooter\\.py" (buffer-name))
             (f-exists? (f-join default-directory "doot.toml")))
    (split-window-right)
    (with-selected-window (selected-window)
      (find-file (f-join default-directory "doot.toml")))
    ))

;;-- end doit/doot

;;-- manifest mode

(defvar-local manifest-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)

(defconst manifest-font-lock-keywords
  (list
   ;; backquote (,(rx ) (subexp facename override laxmatch))
   `(,(rx line-start (or "include"
                        "exclude"
                        "recursive-include"
                        "recusive-exclude"
                        "global-include"
                        "global-exclude"
                        "prune"
                        "graft")) (0 'font-lock-builtin-face))
   )
  "Highlighting for manifest-mode"
  )

(defvar manifest-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?: "." st)
    st)
  "Syntax table for the tag-mode")

;;;###autoload
(define-derived-mode manifest-mode fundamental-mode
  "manifest"
  (interactive)
  (kill-all-local-variables)
  (use-local-map manifest-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list manifest-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'manifest-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'manifest-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table manifest-mode-syntax-table)
  ;;
  (setq major-mode 'manifest-mode)
  (setq mode-name "manifest")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )

;;-- end manifest mode
