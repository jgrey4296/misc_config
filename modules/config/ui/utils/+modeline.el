;;; +modeline.el -*- lexical-binding: t; -*-

;; defined segments are in doom-modeline-fn-alist

;; format-mode-line
;; doom-modeline-def-env
;; doom-modeline-def-segment

(doom-modeline-def-segment vbar
  "a vertical bar"
  " | "
  )

(doom-modeline-def-segment test-segment
  "Testing"
  "blah"
  )

;; Define your custom doom-modeline
(doom-modeline-def-modeline 'my-simple-line
  '(;;lhs
    buffer-position
    matches
    modals
    check
    vbar
    test-segment
    vbar
    ;; buffer-info
    ;; remote-host
    ;; selection-info
    ;; major-mode
    )
  '(;;rhs
    ;; misc-info
    ;; minor-modes
    ;; input-method
    ;; buffer-encoding
    lsp
    major-mode
    vcs
    ;; process
    ;; vcs
    ;; checker
    ))

;; Set default mode-line
;; (add-hook 'doom-modeline-mode-hook
;;         (lambda ()
;;                 (doom-modeline-set-modeline 'my-simple-line 'default)))

;; Configure other mode-lines based on major modes
;; (add-to-list 'doom-modeline-mode-alist '(my-mode . my-simple-line))

;; Or disable other mode-lines
;; (setq 'doom-modeline-mode-alist nil)

(defvar doom-modeline-formatters nil)

(defun doom-modeline--get-formatters ()
  (cl-do-all-symbols (sym)
    (when (s-starts-with? "doom-modeline-format--" (symbol-name sym))
      (add-to-list 'doom-modeline-formatters (s-chop-prefixes '("doom-modeline-format--") (symbol-name sym)))
      )
    )
  )



(defun +jg-ui-modeline-choose (arg)
  (interactive "P")
  (when (or arg (not doom-modeline-formatters))
    (doom-modeline--get-formatters))
  (doom-modeline-set-modeline (intern-soft (ivy-read "Modeline: " doom-modeline-formatters)))
  )

;; todo mode-line-format

(map! :map jg-help-map
      :after jg-help-bindings
      :prefix "u"
      "1" #'+jg-ui-modeline-choose
      )
