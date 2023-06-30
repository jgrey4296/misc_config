;;; +vars.el -*- lexical-binding: t; -*-


(defvar +sh-builtin-keywords
  '("cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git" "grep"
    "kill" "less" "ln" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd" "rm"
    "sleep" "sudo" "touch")
  "A list of common shell commands to be fontified especially in `sh-mode'.")


;;-- specs
(spec-handling-add! lookup-handler '(sh-mode :documentation #'+sh-lookup-documentation-handler))
(spec-handling-add! docsets '(sh-mode "Bash"))
(spec-handling-add! auto-modes
                    '(sh
("\\.bats\\'" . sh-mode)
("\\.\\(?:zunit\\|env\\)\\'" . sh-mode)
("/bspwmrc\\'" . sh-mode)
                      )
                    )

  (spec-handling-add! company
                      '(sh-mode (:mode . #'company-shell) (:mode .  #'company-files))
                      )
(set-electric! 'sh-mode :words '("else" "elif" "fi" "done" "then" "do" "esac" ";;"))
(set-formatter! 'shfmt '("shfmt" "-ci"
                         ("-i" "%d" (unless indent-tabs-mode tab-width))
                         ("-ln" "%s" (pcase sh-shell (`bash "bash") (`mksh "mksh") (_ "posix"))))
  )
(set-repl-handler! 'sh-mode #'+sh/open-repl)
(set-ligatures! 'sh-mode
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
                :dot "." :dot "source")

;;-- end specs
