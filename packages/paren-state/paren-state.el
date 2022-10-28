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
    )
;; (evil-add-hjkl-bindings global-map 'paren)
(suppress-keymap evil-paren-state-map)
(evil-global-set-key 'paren "h" #'sp-beginning-of-sexp)
(evil-global-set-key 'paren "l" #'sp-end-of-sexp)
(evil-global-set-key 'paren "k" #'sp-up-sexp)
(evil-global-set-key 'paren "j" #'sp-down-sexp)

(evil-global-set-key 'paren "p" #'sp-beginning-of-next-sexp)
(evil-global-set-key 'paren "u" #'sp-beginning-of-previous-sexp)
(evil-global-set-key 'paren "o" #'sp-end-of-next-sexp)
(evil-global-set-key 'paren "i" #'sp-end-of-previous-sexp)

;; (evil-global-set-key 'paren "a" #'sp-previous-sexp)
;; (evil-global-set-key 'paren "f" #'sp-next-sexp)
;; (evil-global-set-key 'paren "x" #'sp-backward-parallel-sexp)
;; (evil-global-set-key 'paren "x" #'sp-backward-sexp)
;; (evil-global-set-key 'paren "x" #'sp-forward-parallel-sexp)
;; (evil-global-set-key 'paren "x" #'sp-forward-sexp)

(provide 'paren-state)
