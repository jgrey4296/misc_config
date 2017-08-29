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
(eval-after-load 'auto-complete '(global-auto-complete-mode 1))
(eval-after-load 'flycheck '(global-flycheck-mode 1))

;;(require 'trie-mode)
;no longer needed with outline and occur:
;(autoload "folding.el") ;'nomessage 'noerror)



;(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;(setq auto-mode-alist
;      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))





(global-auto-revert-mode 1)  ;auto revert buffers when changed on disk
(show-paren-mode t)          ;visualize()
;(iswitchb-mode t)            ;use advanced tab switching
(blink-cursor-mode -1)       ;no cursor blinking
(tool-bar-mode -1)           ;disable the awful toolbar
(menu-bar-mode -1)           ;no menu, you can toggle it with C-c m
;(scroll-bar-mode -1)         ;disable the sroll bar



;org mode stuff:
