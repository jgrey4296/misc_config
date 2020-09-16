;;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-
;; Main
(map! :leader

      "SPC" #'evil-avy-goto-line
      ;; Misc
      :desc "Evaluate line/region"  "e"   #'+eval/line-or-region
      :desc "Eval expression"       ";"   #'pp-eval-expression
      :desc "M-x"                   ":"   #'execute-extended-command
      :desc "Org Capture"           "X"   #'org-capture
      ;; C-u is used by evil
      :desc "Universal argument"    "u"   #'universal-argument
      :desc "help"                  "h"    help-map
      :desc "Split Window"          "/"    #'split-window-right
      ;; TODO pop shell

      )

;; Toggles
(map! :leader
      :prefix ("t" . "Toggle")
      :desc "Whitespace" "w"#'whitespace-mode
      ;; centre point/line
      ;; highlight long lines
      ;; auto-completion
      ;; camel-case-motion
      ;;
      ;; fill-column indicator
      ;; indent-guide
      ;; truncate lines
      ;; line numbers
      )

;; Windows
(map! :leader
      :prefix ("w" . "Windows")
       :desc "Delete Window" "d" #'delete-window
       :desc "Split To Right" "/" #'split-window-right
       :desc "Split Below" "-" #'split-window-below
       )

(map! :leader
      :prefix ("b" . "Buffer")
      :desc "Kill Buffer" "d" #'(lambda () (interactive) (kill-buffer (current-buffer)))
      :desc "Pop up scratch buffer" "x"   #'doom/open-scratch-buffer
      :desc "Mark" "m" #'mark-whole-buffer
      )

;; Jumping
(map! :leader
      :prefix ("j" . "Jump")
      :desc "Jump to Line"                          "l"   #'evil-avy-goto-line
      :desc "Jump to definition"                    "d"  #'+lookup/definition
      :desc "Jump to references"                    "D"  #'+lookup/references
      :desc "Find implementations"                  "i"  #'+lookup/implementations
      :desc "Jump to documentation"                 "k"  #'+lookup/documentation
      :desc "Find type definition"                  "t"  #'+lookup/type-definition
      (:when (featurep! :completion ivy)
       :desc "Jump to symbol in current workspace" "j"  #'lsp-ivy-workspace-symbol
       :desc "Jump to symbol in any workspace"     "J"  #'lsp-ivy-global-workspace-symbol)
      (:when (featurep! :completion helm)
       :desc "Jump to symbol in current workspace" "j"  #'helm-lsp-workspace-symbol
       :desc "Jump to symbol in any workspace"     "J"  #'helm-lsp-global-workspace-symbol)
      )

;; Local Mode
;; (:prefix ("m" . "Local Mode"))

(map! :leader
      :when (featurep! :editor evil)
      :prefix ("x" . "Text")
      :v "TAB" #'evil-indent
      )
;; align
;; justify
;; upcase, downcase
;; mark buffer
;; mark line
;; string inflection and surround

(map! :leader
      :prefix ("f" . "file")
      :desc "Find File" "f" #'find-file
      )

;; REGISTERS


(map! :leader
      :prefix ("q" . "quit")
      :desc "Quit" "q" #'kill-emacs
      :desc "Restart" "r" #'restart-emacs
      )

(defun +default-disable-delete-selection-mode-h ()
  (delete-selection-mode -1))
(add-hook 'evil-insert-state-entry-hook 'delete-selection-mode)
(add-hook 'evil-insert-state-exit-hook  '+default-disable-delete-selection-mode-h)

(map! :map universal-argument-map
      :prefix doom-leader-key     "u"#'universal-argument-more
      :prefix doom-leader-alt-key "u"#'universal-argument-more)

(add-transient-hook! 'dired-mode-hook
  (evil-define-key 'normal dired-mode-map (kbd "o") #'dired-find-file-other-window)
  )

(when (featurep! :editor evil +everywhere)
  ;; NOTE SPC u replaces C-u as the universal argument.

  ;; Minibuffer
  (map! :map (evil-ex-completion-map evil-ex-search-keymap)
        "C-a" #'evil-beginning-of-line
        "C-b" #'evil-backward-char
        "C-f" #'evil-forward-char
        :gi "C-j" #'next-complete-history-element
        :gi "C-k" #'previous-complete-history-element)

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

  ;; For folks with `evil-collection-setup-minibuffer' enabled
  (define-key! :states 'insert :keymaps +default-minibuffer-maps
    "C-j"    #'next-line
    "C-k"    #'previous-line)

  (define-key! read-expression-map
    "C-j" #'next-line-or-history-element
    "C-k" #'previous-line-or-history-element) )

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))

;; Help bindings
(map! (:after help :map help-mode-map
       :n "o"       #'link-hint-open-link)
      (:after helpful :map helpful-mode-map
       :n "o"       #'link-hint-open-link)
      (:after info :map Info-mode-map
       :n "o"       #'link-hint-open-link)
      (:after apropos :map apropos-mode-map
       :n "o"       #'link-hint-open-link
       :n "TAB"     #'forward-button
       :n [tab]     #'forward-button
       :n [backtab] #'backward-button)
      (:after view :map view-mode-map
       [escape]  #'View-quit-all)
      (:after man :map Man-mode-map
       :n "q"    #'kill-current-buffer)
      (:after geiser-doc :map geiser-doc-mode-map
       :n "o"    #'link-hint-open-link)

      (:after (evil-org evil-easymotion)
       :map evil-org-mode-map
       :m "gsh" #'+org/goto-visible)

      ;; misc
      :n "C-S-f"  #'toggle-frame-fullscreen
      :n "C-+"    #'doom/reset-font-size
      ;; Buffer-local font resizing
      :n "C-="    #'text-scale-increase
      :n "C--"    #'text-scale-decrease
      ;; Frame-local font resizing
      :n "M-C-="  #'doom/increase-font-size
      :n "M-C--"  #'doom/decrease-font-size)

;;; :completion
(map! (:when (featurep! :completion company)
       :i "C-@"      (cmds! (not (minibufferp)) #'+company/complete)
       :i "C-SPC"    (cmds! (not (minibufferp)) #'+company/complete)
       (:after company
        (:map company-active-map
         "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
         "C-n"     #'company-select-next
         "C-p"     #'company-select-previous
         "C-j"     #'company-select-next
         "C-k"     #'company-select-previous
         "C-h"     #'company-show-doc-buffer
         "C-u"     #'company-previous-page
         "C-d"     #'company-next-page
         "C-s"     #'company-filter-candidates
         "C-S-s"   (cond ((featurep! :completion helm) #'helm-company)
                         ((featurep! :completion ivy)  #'counsel-company))
         "C-SPC"   #'company-complete-common
         "TAB"     #'company-complete-common-or-cycle
         [tab]     #'company-complete-common-or-cycle
         [backtab] #'company-select-previous
         [f1]      nil)
        (:map company-search-map  ; applies to `company-filter-map' too
         "C-n"     #'company-select-next-or-abort
         "C-p"     #'company-select-previous-or-abort
         "C-j"     #'company-select-next-or-abort
         "C-k"     #'company-select-previous-or-abort
         "C-s"     (cmd! (company-search-abort) (company-filter-candidates))
         [escape]  #'company-search-abort)))

      (:when (featurep! :completion ivy)
       (:after ivy
        :map ivy-minibuffer-map
        "C-SPC" #'ivy-call-and-recenter  ; preview file
        "C-l"   #'ivy-alt-done
        "C-v"   #'yank)
       (:after counsel
        :map counsel-ag-map
        "C-SPC"    #'ivy-call-and-recenter ; preview
        "C-l"      #'ivy-done
        [C-return] #'+ivy/git-grep-other-window-action))

      (:when (featurep! :completion helm)
       (:after helm :map helm-map
        [remap next-line]     #'helm-next-line
        [remap previous-line] #'helm-previous-line
        [left]     #'left-char
        [right]    #'right-char
        "C-S-f"    #'helm-previous-page
        "C-S-n"    #'helm-next-source
        "C-S-p"    #'helm-previous-source
        (:when (featurep! :editor evil +everywhere)
         "C-j"    #'helm-next-line
         "C-k"    #'helm-previous-line
         "C-S-j"  #'helm-next-source
         "C-S-k"  #'helm-previous-source)
        "C-u"      #'helm-delete-minibuffer-contents
        "C-s"      #'helm-minibuffer-history
        ;; Swap TAB and C-z
        "TAB"      #'helm-execute-persistent-action
        [tab]      #'helm-execute-persistent-action
        "C-z"      #'helm-select-action)
       (:after helm-ag :map helm-ag-map
        "C--"      #'+helm-do-ag-decrease-context
        "C-="      #'+helm-do-ag-increase-context
        [left]     nil
        [right]    nil)
       (:after helm-files :map (helm-find-files-map helm-read-file-map)
        [C-return] #'helm-ff-run-switch-other-window
        "C-w"      #'helm-find-files-up-one-level)
       (:after helm-locate :map helm-generic-files-map
        [C-return] #'helm-ff-run-switch-other-window)
       (:after helm-buffers :map helm-buffer-map
        [C-return] #'helm-buffer-switch-other-window)
       (:after helm-occur :map helm-occur-map
        [C-return] #'helm-occur-run-goto-line-ow)
       (:after helm-grep :map helm-grep-map
        [C-return] #'helm-grep-run-other-window-action)))

;;; :ui
(map! (:when (featurep! :ui popup)
       "C-`"   #'+popup/toggle
       "C-~"   #'+popup/raise
       "C-x p" #'+popup/other)

      (:when (featurep! :ui workspaces)
       :n "C-t"   #'+workspace/new
       :n "C-S-t" #'+workspace/display
       :g "M-1"   #'+workspace/switch-to-0
       :g "M-2"   #'+workspace/switch-to-1
       :g "M-3"   #'+workspace/switch-to-2
       :g "M-4"   #'+workspace/switch-to-3
       :g "M-5"   #'+workspace/switch-to-4
       :g "M-6"   #'+workspace/switch-to-5
       :g "M-7"   #'+workspace/switch-to-6
       :g "M-8"   #'+workspace/switch-to-7
       :g "M-9"   #'+workspace/switch-to-8
       :g "M-0"   #'+workspace/switch-to-final
       (:when IS-MAC
        :g "s-t"   #'+workspace/new
        :g "s-T"   #'+workspace/display
        :n "s-1"   #'+workspace/switch-to-0
        :n "s-2"   #'+workspace/switch-to-1
        :n "s-3"   #'+workspace/switch-to-2
        :n "s-4"   #'+workspace/switch-to-3
        :n "s-5"   #'+workspace/switch-to-4
        :n "s-6"   #'+workspace/switch-to-5
        :n "s-7"   #'+workspace/switch-to-6
        :n "s-8"   #'+workspace/switch-to-7
        :n "s-9"   #'+workspace/switch-to-8
        :n "s-0"   #'+workspace/switch-to-final)))

;;; :editor
(map! (:when (featurep! :editor format)
        :n "gQ" #'+format:region)

      (:when (featurep! :editor rotate-text)
        :n "!"  #'rotate-text)

      (:when (featurep! :editor snippets)
        ;; auto-yasnippet
        :i  [C-tab] #'aya-expand
        :nv [C-tab] #'aya-create))

;;; :tools
(when (featurep! :tools eval)
  (map! "M-r" #'+eval/buffer))
