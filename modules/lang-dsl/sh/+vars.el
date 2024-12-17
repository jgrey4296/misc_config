;;; +vars.el -*- lexical-binding: t; -*-


(defvar +sh-builtin-keywords
  '("cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git" "grep"
    "kill" "less" "ln" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd" "rm"
    "sleep" "sudo" "touch")
  "A list of common shell commands to be fontified especially in `sh-mode'.")


;;-- specs
(speckler-add! lookup-handler '(sh-mode :documentation #'+sh-lookup-documentation-handler))
(speckler-add! docsets '(sh-mode "Bash"))
(speckler-add! auto-modes
                    '(sh
                      ("\\.bash" . bash-ts-mode)
                      ("\\.bats\\'" . sh-mode)
                      ("\\.\\(?:zunit\\|env\\)\\'" . sh-mode)
                      ("/bspwmrc\\'" . sh-mode)
                      )
                    )
(speckler-add! company
                      `(sh-mode (:mode company-shell) (:mode company-files))
                      )
(speckler-add! electric
                    '(sh-mode
                      :words ("else" "elif" "fi" "done" "then" "do" "esac" ";;")
                      )
                    )
(speckler-add! repl
                    '(sh-mode :start +sh/open-repl)
                    )
(speckler-add! ligatures
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

;;-- end specs
