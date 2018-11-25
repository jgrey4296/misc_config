;Modes I want on by default:
(global-hl-line-mode 1)
(menu-bar-mode -1)
(winner-mode 1)
(show-paren-mode 1)
(global-linum-mode 1)
(column-number-mode 1)
(yas-global-mode 1)
(icy-mode 1)
(outline-minor-mode)
(org-mode)
(global-auto-revert-mode 1)  ;auto revert buffers when changed on disk
(show-paren-mode t)          ;visualize()
(blink-cursor-mode -1)       ;no cursor blinking
(tool-bar-mode -1)           ;disable the awful toolbar
(menu-bar-mode -1)           ;no menu, you can toggle it with C-c m
;(iswitchb-mode t)           ;use advanced tab switching
;(scroll-bar-mode -1)        ;disable the sroll bar
(pinentry-start)             ;starts pinentry for gpg use in terminal


(eval-after-load 'auto-complete '(global-auto-complete-mode 1))
(eval-after-load 'flycheck '(global-flycheck-mode 1))

(require 'chuck-mode)
;;(require 'python-django)
;;(require 'pony-mode)
(require 'netlogo-mode)
(require 'sclang)
(require 'trie-mode)
(require 'tidal)

;;set the tidal interpreter
(setq tidal-interpreter "/usr/local/bin/ghci")
(setq tidal-interpreter-arguments
      (list "-ghci-script" (expand-file-name "~/github/languageLearning/tidal/.ghci")))
            

;;setting up erlang
;; (also has a load path set in root el file)
(setq erlang-root-dir "/usr/local/opt/erlang")
(setq exec-path (cons "usr/local/opt/erlang/bin" exec-path))
(require 'erlang-start)

(autoload 'drools-mode "drools-mode")
;(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;(setq auto-mode-alist
;      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

