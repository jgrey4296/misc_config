;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-
(message "Setting up Leaderless bindings: %s" (current-time-string))
(global-set-key (kbd "C-c u") #'universal-argument)

;; For minibuffer use:
(map! :map ctl-x-map
      "[" "("
      "]" ")")

(map! :map universal-argument-map
      :prefix doom-leader-key     "u" #'universal-argument-more
      :prefix doom-leader-alt-key "u" #'universal-argument-more)

;;
;; Shell
(map! :after shell
      :map shell-mode-map
      "C-d" #'comint-send-eof
      :localleader
      "h" #'counsel-shell-history)

(evil-make-intercept-map shell-mode-map)

;; Comint
;; overrides the default normal mode binding of evil-ret
(map! :after comint
      :map comint-mode-map
      "C-d" #'comint-send-eof
      :n "RET" #'comint-send-input
      )

(evil-make-intercept-map comint-mode-map)

;; Flycheck
(map! :after flycheck
      :map flycheck-error-list-mode-map
      :n "," nil
      :n "," #'tabulated-list-sort
      :n "{" #'tabulated-list-narrow-current-column
      :n "}" #'tabulated-list-widen-current-column
      )

;; Git Timemachine
(map! :after git-timemachine
      :map git-timemachine-mode-map
      :n "[ g" #'git-timemachine-show-previous-revision
      :n "] g" #'git-timemachine-show-next-revision
      )

;; Snipe
(map! :after evil-snipe
      :map evil-snipe-mode-map
      :nm "S" nil
      :nm "s" nil
      )

(map! :map special-mode-map
      :n "q" #'quit-window
      )

;; Minibuffer
(define-key! :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit
  "C-a"    #'move-beginning-of-line
  "C-r"    #'evil-paste-from-register
  "C-u"    #'evil-delete-back-to-indentation
  "C-v"    #'yank
  "C-w"    #'doom/delete-backward-word
  "C-z"    (cmd! (ignore-errors (call-interactively #'undo))))

(define-key! :keymaps +default-minibuffer-maps
  "C-j"    #'next-line
  "C-k"    #'previous-line
  "C-S-j"  #'scroll-up-command
  "C-S-k"  #'scroll-down-command)

;; Popup
(map! (:when (featurep! :ui popup)
       "C-`"   #'+popup/toggle
       "C-~"   #'+popup/raise
       "C-p" #'+popup/other)
      )

;; LSP
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

;; Messages
(map! :after message
      :map messages-buffer-mode-map
      :g "0" #'evil-beginning-of-line
      )

(evil-make-overriding-map messages-buffer-mode-map)

;; Mouse Deactivation
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

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))
