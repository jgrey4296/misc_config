;;; paren-state.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'evil)
(require 'smartparens)

(evil-define-state paren
  "Paren State"
  :tag "<P>"
  :message "-- Paren --"
  ;; :enable (motion)
  ;; :input-method t
  ;; :suppress-keymap t
  (setq-local sp-navigate-reindent-after-up nil
              sp-navigate-reindent-after-up-in-string nil
              )
  )

;; (evil-add-hjkl-bindings global-map 'paren)
(suppress-keymap evil-paren-state-map)
(evil-global-set-key 'paren "H" #'sp-beginning-of-sexp)
(evil-global-set-key 'paren "h" #'sp-backward-up-sexp)
(evil-global-set-key 'paren "l" #'sp-down-sexp)
(evil-global-set-key 'paren "L" #'sp-end-of-sexp)
(evil-global-set-key 'paren "k" #'sp-backward-sexp)
(evil-global-set-key 'paren "K" #'sp-up-sexp)
(evil-global-set-key 'paren "j" #'sp-next-sexp)

(evil-global-set-key 'paren "p" #'sp-beginning-of-next-sexp)
(evil-global-set-key 'paren "u" #'sp-beginning-of-previous-sexp)
(evil-global-set-key 'paren "o" #'sp-end-of-next-sexp)
(evil-global-set-key 'paren "i" #'sp-end-of-previous-sexp)
(evil-global-set-key 'paren "q" #'evil-normal-state)

;; (evil-global-set-key 'paren "a" #'sp-previous-sexp)
;; (evil-global-set-key 'paren "f" #'sp-next-sexp)
;; (evil-global-set-key 'paren "x" #'sp-backward-parallel-sexp)
;; (evil-global-set-key 'paren "x" #'sp-backward-sexp)
;; (evil-global-set-key 'paren "x" #'sp-forward-parallel-sexp)
;; (evil-global-set-key 'paren "x" #'sp-forward-sexp)

(provide 'paren-state)
