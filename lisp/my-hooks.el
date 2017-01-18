;;context highlighting
(add-hook 'js2-mode-hook #'context-coloring-mode)

;; Emacs Lisp:
(add-hook 'emacs-lisp-mode-hook #'context-coloring-mode)

;; eval-expression:
(add-hook 'eval-expression-minibuffer-setup-hook #'context-coloring-mode) ; 24.4+
;;(add-hook 'minibuffer-setup-hook #'context-coloring-mode)                 ; 24.3


;;Add Hooks to enable spell checking for text and latex
;;Requires flyspell and ispell to be installed
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(latex-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))


(dolist (hook '(occur-mode-hook))
  (add-hook hook(lambda() (linum-mode 0))))

(add-hook 'csharp-mode-hook
          (lambda ()
            (local-set-key (kbd "{") 'c-electric-brace)))

;;filtering out exit characters from node:
(add-hook 'comint-preoutput-filter-functions
          (lambda(output)
            (replace-regexp-in-string "\\[[0-9]+[GKJ]" "" output)))


;; (add-hook 'outline-mode-hook
;;           (lambda ()
;;             (require 'outline-cycle)))

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)))


(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;(add-hook 'after-init-hook 'global-flycheck-mode)

;; (add-hook 'php-mode-hook 'flycheck-mode)
;; (add-hook 'sh-mode-hook 'flycheck-mode)
;; (add-hook 'json-mode-hook 'flycheck-mode)
;; (add-hook 'nxml-mode-hook 'flycheck-mode)
;; (add-hook 'python-mode-hook 'flycheck-mode)
;; (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
;; (add-hook 'lisp-interaction-mode-hook 'flycheck-mode)


;addL javascript, racket, perl, prolog, csharp, haskell hooks


(dolist (hook '(css-mode-hook
                html-mode-hook
                js-mode-hook
                emacs-lisp-mode-hook
                org-mode-hook
                text-mode-hook
                )))


;; (add-hook hook 'rainbow-mode))

;ibuffer grouping:
(setq ibuffer-saved-filter-groups
      (quote (("home"
               ("emacs" (or
                         (name . "^\\*shell\\*$")
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("Lisp" (name . ".\.el$"))
               ("Javascript" (or
                              (mode . js-mode)
                              (name . ".\.js$")))
               ("Python" (name . ".\.py$"))
               ("Chuck" (name . ".\.ck$"))
               ("Supercollider" (name . ".\.sc$"))
               ("Csound" (name . ".\.csd$"))
               ("Haskell" (name . ".\.hs$"))
               ("Headers" (name . ".\.h$"))
               ("C++" (mode . c++-mode))
               ("C" (name . ".\.c$"))
               ("C#" (name . ".\cs"))
               ("Perl" (mode . perl-mode))
               ("Web" (or
                       (name . ".\.html$")
                       (name . ".\.css$")))
               ("Notes" (mode . org-mode))
               ("Dired" (mode . dired-mode))))))
               
(dolist (hook '(ibuffer-mode-hook))
  (add-hook hook
            (lambda() (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "home")
              )))
