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
  (add-hook hook (lambda () (flyspell-mode 1)))
  (add-hook hook (lambda () (writegood-mode 1)))
  )

(dolist (hook '(latex-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1)))
  (add-hook hook (lambda () (writegood-mode 1)))
  )

(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1)))
  (add-hook hook (lambda () (writegood-mode 1)))
  )

(dolist (hook '(markdown-mode))
  (add-hook hook (lambda () (flyspell-mode 1)))
  (add-hook hook (lambda () (writegood-mode 1)))
  )

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


(dolist (hook '(css-mode-hook
                html-mode-hook
                js-mode-hook
                emacs-lisp-mode-hook
                org-mode-hook
                text-mode-hook
                )))




;ibuffer grouping:
(setq ibuffer-saved-filter-groups
      (quote (("home"
               ("emacs" (or
                         (name . "^\\*shell\\*$")
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("Lisp" (name . ".\\.el$"))
               ("Web" (or
                       (mode . html-mode)
                       (mode . css-mode)
                       (mode . less-css-mode)                             
                       (name . ".\\.html$")
                       (name . ".\\.css$")
                       (name . ".\\.less$")                       
                       ))
               ("Javascript" (or
                              (mode . js-mode)
                              (mode . jsx-mode)
                              (mode . js2-mode)
                              (name . ".\\.js$")
                              (name . ".\\.jsx$")
                              (name . ".\\.ejs$")))
               ("Json" (or
                        (mode . json-mode)
                        (name . ".\\.json$")))                             
               ("Python" (name . ".\\.py$"))
               ("Haskell" (name . ".\\.hs$"))
               ("Chuck" (name . ".\\.ck$"))
               ("Supercollider" (name . ".\\.sc$"))
               ("Csound" (name . ".\\.csd$"))
               ("Headers" (name . ".\\.h$"))
               ("C" (name . ".\\.c$"))
               ("C++" (mode . c++-mode))
               ("C#" (name . ".\cs$"))
               ("Perl" (mode . perl-mode))
               ("Notes" (mode . org-mode))
               ("Dired" (mode . dired-mode))))))
               
(dolist (hook '(ibuffer-mode-hook))
  (add-hook hook
            (lambda() (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "home")
               )))

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8db" (buffer-size)))))

;; Modify the default ibuffer-formats
  (setq ibuffer-formats
	'((mark modified read-only " "
		(name 18 18 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 16 16 :left :elide)
		" "
		filename-and-process)))
