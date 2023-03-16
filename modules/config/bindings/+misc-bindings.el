;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-
(message "Setting up Leaderless bindings: %s" (current-time-string))
(global-set-key (kbd "C-c u") #'universal-argument)

(map! :map global-map
      "<f1>" #'ignore
      "<f2>" #'ignore
      "<f3>" #'ignore
      "<f4>" #'ignore
      "<f5>" #'ignore
      "<f6>" #'ignore
      "<f10>" #'ignore
      )

;; For minibuffer use:
(map! :map ctl-x-map
      "[" "("
      "]" ")")

(map! :map universal-argument-map
      :prefix doom-leader-key     "u" #'universal-argument-more
      :prefix doom-leader-alt-key "u" #'universal-argument-more)

(map! :map special-mode-map
      :n "q" #'quit-window
      )

;;-- shell
(map! :after shell
      :map shell-mode-map
      "C-d" #'comint-send-eof
      :localleader
      "h" #'counsel-shell-history)

;;-- end shell

;;-- comint
;; overrides the default normal mode binding of evil-ret
(map! :after comint
      :map comint-mode-map
      "C-d" #'comint-send-eof
      :n "RET" #'comint-send-input
      )

;;-- end comint

;;-- flycheck
(map! :after flycheck
      :map flycheck-error-list-mode-map
      :n "," nil
      :n "," #'tabulated-list-sort
      :n "{" #'tabulated-list-narrow-current-column
      :n "}" #'tabulated-list-widen-current-column
      )

;;-- end flycheck

;;-- evil snipe
(map! :after evil-snipe
      :map evil-snipe-mode-map
      :nm "S" nil
      :nm "s" nil
      )

;;-- end evil snipe

;;-- minibuffer
(define-key! :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit
  "C-a"    #'move-beginning-of-line
  "C-r"    #'evil-paste-from-register
  "C-u"    #'evil-delete-back-to-indentation
  "C-v"    #'yank
  "C-w"    #'doom/delete-backward-word
  "C-z"    (cmd! (ignore-errors (call-interactively #'undo)))
  )

(define-key! :keymaps +default-minibuffer-maps
  "C-j"    #'next-line
  "C-k"    #'previous-line
  "C-S-j"  #'scroll-up-command
  "C-S-k"  #'scroll-down-command)

;;-- end minibuffer

;;-- lsp
(map! :after lsp-mode
      :map lsp-command-map
      (:prefix ("w" . "Workspaces"))
      (:prefix ("=" . "Formatting"))
      (:prefix ("F" . "Folders"))
      (:prefix ("T" . "Toggles"))
      (:prefix ("g" . "Goto"))
      (:prefix ("h" . "Help"))
      (:prefix ("r" . "Refactoring"))
      (:prefix ("a" . "Actions"))
      (:prefix ("G" . "Peek"))
      )

;;-- end lsp

;;-- messages
(map! :after message
      :map messages-buffer-mode-map
      :g "0" #'evil-beginning-of-line
      )

;;-- end messages

;;-- evil overrides/intercept
(evil-make-overriding-map messages-buffer-mode-map)
(evil-make-intercept-map comint-mode-map)
(evil-make-intercept-map shell-mode-map)
(evil-make-intercept-map read-expression-map)

;;-- end evil overrides/intercept

;;-- Mouse Deactivation
(define-key evil-motion-state-map [down-mouse-1] #'ignore)
(define-key evil-motion-state-map [mouse-1] #'ignore)
(define-key evil-motion-state-map [drag-mouse-1] #'ignore)

(define-key evil-motion-state-map [down-mouse-2] #'ignore)
(define-key evil-motion-state-map [mouse-2] #'ignore)
(define-key evil-motion-state-map [drag-mouse-2] #'ignore)

(define-key evil-motion-state-map [down-mouse-3] #'ignore)
(define-key evil-motion-state-map [mouse-3] #'ignore)
(define-key evil-motion-state-map [drag-mouse-3] #'ignore)

(define-key evil-motion-state-map [mouse-4] #'ignore)
(define-key evil-motion-state-map [mouse-5] #'ignore)
;;-- end Mouse Deactivation

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))
