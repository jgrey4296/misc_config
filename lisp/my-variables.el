;; Variable Setting:
(setq-default indent-tabs-mode nil
              backup-directory-alist `(("." .  "~/.emacs.d/backups/"))
              transient-mark-mode t
              default-directory (expand-file-name "~/")
              default-auto-fill-function `do-auto-fill
              inhibit-startup-screen 1
              major-mode `org-mode
              ;;Variables for sclang
              sclang-library-configuration-file (expand-file-name "~/.emacs.setup/setup_files/sclang.yaml")
              sclang-udp-port 57120
              ;;set the tidal interpreter
              tidal-interpreter "/usr/local/bin/ghci"
              tidal-interpreter-arguments
              (list "-ghci-script" (expand-file-name "~/github/languageLearning/tidal/.ghci"))
              ;;setting up erlang
              ;; (also has a load path set in root el file)
              erlang-root-dir "/usr/local/opt/erlang"
              exec-path (cons "/usr/local/opt/erlang/bin" exec-path)
              ;;dropdown list use for yas
              yas-prompt-functions '(yas-dropdown-prompt
                                     yas-ido-prompt
                                     yas-completing-prompt)
              yas-snippet-dirs `( ,(expand-file-name "~/.emacs.setup/snippets/")
                                 ,(expand-file-name "~/github/otherLibs/yasnippet-snippets/snippets")
                                 ,(expand-file-name "~/github/otherLibs/yasnippet-snippets"))
              ;;personal iSpell dictionary
              ispell-personal-dictionary (expand-file-name "~/.emacs.setup/setup_files/.ispell_english")
              ;;Custom Scratch Buffer
              scratchBufferFile (expand-file-name "~/.emacs.setup/setup_files/scratch")
              initial-scratch-message (file-string scratchBufferFile)
              ;;flyspell-issue-message-flag nil)
              ;;IBuffer:
              ibuffer-never-show-predicates `("\\.ds_store")
              ;;dired:
              completion-ignored-extensions (cons ".DS_Store" completion-ignored-extensions)
              dired-omit-files "^\\.?#\\|^\\.$\\|^\\.DS_Store$\\|^\\.git$\\|^__pycache__$"
              ;;dired-omit-extensions (cons ".DS_Store" dired-omit-extensions)
              ;;shell setup:
              ;;explicit-shell-file-name "/opt/local/bin/bash"
              shell-file-name "bash"
              ;;explicit-bash-args ""
              ;;use spaces instead of tabs
              tab-width 4
              indent-tabs-mode nil 
              ;;"tab" with in c-related modes
              c-basic-offset 4     
              ;;delete more than one space
              c-hungry-delete-key t 
              ;;disable the annoying doc checker
              flycheck-disabled-checkers '(emacs-lisp-checkdoc)
              flycheck-indication-mode 'right-fringe
              ;SCLANG Variables:
              sclang-auto-scroll-post-buffer t
              sclang-eval-line-forward nil
              sclang-help-path (quote ("/Applications/SuperCollider/Help"))
              sclang-runtime-directory (expand-file-name "~/.sclang/")
              auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
              ;;abbrev-file complaint quieting
              abbrev-file-name "~/.emacs.d/abbrevs_defs"
)              

(setenv "SHELL" shell-file-name)
;adding melpa to package archives:
(when (> emacs-major-version 23)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives 
	       '("melpa" . "http://melpa.milkbox.net/packages/")
	       'APPEND))

;For setting sbcl lisp instead
;;;(setq inferior-lisp-program "/usr/local/bin/sbcl")

(setq ibuffer-formats '((mark modified read-only " " (name 15 18 :left
			      :elide) " " (size 5 5 :right :elide) " "
			      mode)))

(make-directory (expand-file-name "~/.emacs.d/autosaves/") t)
(make-directory (expand-file-name "~/.emacs.d/backups/") t)



;Sweeping blow to use utf8
;http://steckerhalter.co.vu/steckemacs.html#sec-1
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

