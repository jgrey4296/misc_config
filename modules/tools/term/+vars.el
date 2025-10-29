;;; +vars.el -*- lexical-binding: t; -*-

(defvar +sh-builtin-keywords
  '("cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git" "grep"
    "kill" "less" "ln" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd" "rm"
    "sleep" "sudo" "touch")
  "A list of common shell commands to be fontified especially in `sh-mode'.")

;;-- keymaps

(defvar jg-term-shell-mode-map (make-sparse-keymap))

(defvar jg-term-comint-mode-map (make-sparse-keymap))

;;-- end keymaps

;;-- vterm
(setq vterm-kill-buffer-on-exit t
      vterm-max-scrollback 5000
      )

;;-- end vterm

(speckler-setq! shell ()
  shell-dynamic-complete-functions '(
                                     ;; comint-c-a-p-replace-by-expanded-history
                                     ;; shell-c-a-p-replace-by-expanded-directory
                                     shell-environment-variable-completion
                                     shell-command-completion
                                     pcomplete-completions-at-point
                                     shell-filename-completion
                                     ;; comint-filename-completion
                                     )
  shell-completion-fignore nil

  ;; comint-dynamic-complete-functions '(comint-c-a-p-replace-by-expanded-history
  ;;                                     comint-filename-completion)
  comint-completion-addsuffix t
  comint-completion-recexact nil
  comint-completion-autolist nil

  ansi-color-for-comint-mode t

  comint-prompt-read-only t
  comint-buffer-maximum-size 2048 ; double the default
  )

(speckler-add! popup ()
  '(shell
    ("^\\*shell" :side bottom :ttl nil :height 0.3 :quit t :select t :priority 100)
    )
  '(vterm
    ("^\\*vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

    )
  )
(speckler-add! auto-modes ()
  '(shell
    ("\\.bash\\'" . sh-mode)
    ("\\.bash"    . bash-ts-mode)
    ("\\.bats\\'" . sh-mode)
    ("\\.\\(?:zunit\\|env\\)\\'" . sh-mode)
    ("/bspwmrc\\'" . sh-mode)
    )
  )
(speckler-add! babel ()
  '(term
    (:name bash       :lib ob-shell)
    (:name eshell     :lib ob-eshell)
    (:name sh         :lib ob-shell)
    (:name shell      :lib ob-shell)
    )
  )
(speckler-add! org-src ()
  '(term
    ("bash" . sh)
    ("shell" . sh)
    ("sh" . sh)
    ("bash2" . sh)
    )
  )
(speckler-add! company ()
  '(shell-mode (:mode company-shell company-shell-env company-files))
  `(sh-mode (:mode company-shell) (:mode company-files))
  )
(speckler-add! ivy-actions ()
  '(jg-term-ivy-switch-term
    ("k" +jg-ivy-kill-buffer "Kill")
    )
  )
(speckler-add! treesit-source ()
  '(bash          "git@github.com:tree-sitter/tree-sitter-bash.git")
  )
(speckler-add! doc-lookup ()
  '(sh-mode :documentation #'+sh-lookup-documentation-handler)
  )
(speckler-add! docsets ()
  '(sh-mode "Bash")
  )
(speckler-add! electric ()
  '(sh-mode
    :words ("else" "elif" "fi" "done" "then" "do" "esac" ";;")
    )
  )
(speckler-add! repl ()
  '(sh-mode :start +sh/open-repl)
  )
(speckler-add! ligatures ()
  '(sh-mode
    ;; Functional
    :def "function"
    ;; Types
    :true "true" :false "false"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :in "in"
    :for "for"
    :return "return"
    ;; Other
    :dot "." :dot "source"
    )
  )
(speckler-add! treesit-bin-override ()
  '(bash :lib-base "bash" :entry-func "tree_sitter_bash")
  )
(speckler-add! tree-sit-lang ()
  '(sh-mode         . bash)
  )
(speckler-add! file-templates ()
  :override t
  '(sh
    ("\\.bash\\'"  :trigger "__" :mode sh-mode)
    ("\\.envrc\\'" :trigger "__envrc" :mode sh-mode :priority 200)
    )
  )
