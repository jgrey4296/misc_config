;Variable Setting:
(setq indent-tabs-mode nil)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(setq transient-mark-mode t)
(setq default-directory "~/")
(setq default-auto-fill-function `do-auto-fill)
(setq inhibit-startup-screen 1)

(setq-default major-mode `org-mode)

;;Variables for sclang
(setq sclang-library-configuration-file "~/.emacs.setup/setup_files/sclang.yaml")
(setq sclang-udp-port 57120)



;dropdown list use for yas
;;(require 'dropdown-list)
;;(setq yas-prompt-functions '(yas-dropdown-prompt
;;                             yas-ido-prompt
;;                             yas-completing-prompt))


(setq yas-snippet-dirs '("~/.emacs.setup/snippets/"
                         "/Users/johngrey/github/otherLibs/yasnippet-snippets/snippets"
                         "/Users/jgrey/github/otherLibs/yasnippet-snippets"))



;adding melpa to package archives:
(when (> emacs-major-version 23)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives 
	       '("melpa" . "http://melpa.milkbox.net/packages/")
	       'APPEND))

;file association:
<<<<<<< HEAD
(setq auto-mode-alist (cons '(".*\.pde" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".*\.pl" .  prolog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".*\.py" .  python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".*\.emacs" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".*\.js" . js-mode) auto-mode-alist))
(setq auto-mode-alist (cons '(".*\.md" . markdown-mode) auto-mode-alist))
=======
;; (setq auto-mode-alist (cons '(".*\.pde" . java-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '(".*\.lp" .  prolog-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '(".*\.py" .  python-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '(".*\.emacs" . lisp-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '(".*\.js" . js-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '(".*\.md" . markdown-mode) auto-mode-alist))
>>>>>>> 84169cfec733237f76b51950bfeaa340dc2bd8ad

;;(defvar scratchBufferFile (expand-file-name "./scratch.txt"))
;;(setq initial-scratch-message (file-string scratchBufferFile))
(setq flyspell-issue-message-flag nil)

;For setting sbcl lisp instead
;;;(setq inferior-lisp-program "/usr/local/bin/sbcl")

;Ibuffer filter groups:
;need to rework this:
;; (setq ibuffer-saved-filter-groups
;;       '(("home"
;; 	  ("emacs" (filename . ".emacs"))
;; 	  ("JG" (filename . "\*JG\*"))
;; 	 ("Headers" (filename . "\.h$"))
;; 	 ("Code" (or (mode . c-mode) 
;; 		     (mode . c++-mode)
;; 		     (mode . java-mode)
;; 		     (mode . javascript-mode)
;; 		     (filename . "\.js$"))
;; 	  )
;; 	  ("Web" (or (mode . html-mode)
;; 			 (mode . css-mode)))
;; 	      ("Readme" (or (name . "\*README\*")
;; 			      (name . "\*Help\*")
;; 			       (name . "\*Apropos\*")
;; 			            (name . "\*info\*"))))))

(setq ibuffer-formats '((mark modified read-only " " (name 15 18 :left
			      :elide) " " (size 5 5 :right :elide) " "
			      mode)))

(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

;Custom Variables:
(custom-set-variables
 '(sclang-auto-scroll-post-buffer t)
 '(sclang-eval-line-forward nil)
 '(sclang-help-path (quote ("/Applications/SuperCollider/Help")))
 '(sclang-runtime-directory "~/.sclang/")
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/`" t))) 
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 )


;;(custom-set-variables '(haskel-mode-hook '(turn-on-haskell-indentation)))

;shell setup:
;;(setq explicit-shell-file-name "/opt/local/bin/bash")
(setq shell-file-name "bash")
;(setq explicit-bash-args "")
(setenv "SHELL" shell-file-name)

;Sweeping blow to use utf8
;http://steckerhalter.co.vu/steckemacs.html#sec-1
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq-default
 tab-width 4
 indent-tabs-mode nil                   ;use spaces instead of tabs
 c-basic-offset 4                       ;"tab" with in c-related modes
 c-hungry-delete-key t                  ;delete more than one space
 )

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;disable the annoying doc checker
(setq flycheck-indication-mode 'right-fringe)

