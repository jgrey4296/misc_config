;Modes I want on by default:
(global-hl-line-mode 1)
(menu-bar-mode -1)
(winner-mode 1)
(show-paren-mode 1)
(global-linum-mode 1)
(column-number-mode 1)
(yas-global-mode 1)
(outline-minor-mode)
(org-mode)                   ;; Start org first, then icy, or weird things happen
(icy-mode 1)
(global-auto-revert-mode 1)  ;auto revert buffers when changed on disk
(show-paren-mode t)          ;visualize()
(blink-cursor-mode -1)       ;no cursor blinking
(tool-bar-mode -1)           ;disable the awful toolbar
(menu-bar-mode -1)           ;no menu, you can toggle it with C-c m
(pinentry-start)             ;starts pinentry for gpg use in terminal

(autoload 'netlogo-mode "netlogo-mode" :interactive t)
(autoload 'drools-mode "drools-mode" :interactive t)
(require 'chuck-mode)
(require 'sclang)
(require 'trie-mode)
(require 'tidal)
(require 'erlang-start)
;;(require 'dropdown-list)
;;(require 'python-django)
;;(require 'pony-mode)
;;(iswitchb-mode t)           ;use advanced tab switching
;;(scroll-bar-mode -1)        ;disable the sroll bar

(eval-after-load 'auto-complete '(global-auto-complete-mode 1))
(eval-after-load 'flycheck '(global-flycheck-mode 1))

