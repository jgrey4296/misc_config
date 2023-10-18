;;; +vars.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "config.el")
  )

;;-- specs
(spec-handling-add! popup
                    '(lisp
                     ("^\\*Buttercup\\*$" :size 0.45 :select nil :ttl 0)
                     )
                    )
(spec-handling-add! fold
                    `(lisp
                     :modes (emacs-lisp-mode lisp-mode)
                     :priority 25
                     :triggers (:open-all  ,#'hs-show-all
                                :close-all ,#'hs-hide-all
                                :toggle    ,#'hs-toggle-hiding
                                :open      ,#'hs-show-block
                                :open-rec  nil
                                :close     ,#'hs-hide-block
                                )
                     )
                    )
(spec-handling-add! file-templates
                    `(lisp
                     ("minor-mode\\.el\\'" :trigger "__minor-mode" :mode emacs-lisp-mode)
                     ("mode\\.el\\'"       :trigger "__mode"       :mode emacs-lisp-mode)
                     ("ob-.+?\\.el\\'"     :mode emacs-lisp-mode :trigger "__org_babel")
                     ("/.dir-locals.el\\'" :mode emacs-lisp-mode :trigger "__dir_locals")
                     ("-test\\.el\\'"      :mode emacs-lisp-mode  :trigger "__test")
                     (emacs-lisp-mode      :trigger "__package")
                     )
                    )
(spec-handling-add! projects
                    '(emacs-eldev projectile-eldev-project-p :project-file "eldev" :compilation-dir nil :configure nil :compile "eldev compile" :test "eldev test" :install nil :package "eldev package" :run "eldev emacs")
                    '(emacs-cask ("cask") :project-file "cask" :compilation-dir nil :configure nil :compile "cask install" :test nil :install nil :package nil :run nil :test-suffix "-test" :test-prefix "test-")
                    )
(spec-handling-add! rotate-text
                    '(emacs-lisp-mode
                     :symbols (("t" "nil")
                               ("let" "let*")
                               ("when" "unless")
                               ("advice-add" "advice-remove")
                               ("defadvice!" "undefadvice!")
                               ("add-hook" "remove-hook")
                               ("add-hook!" "remove-hook!")
                               ("it" "xit")
                               ("describe" "xdescribe")
                               )
                     )
                    '(racket-mode
                     :symbols (("#true" "#false"))
                     )
                    )
(spec-handling-add! whitespace-cleanup
                    `(emacs-lisp-mode
                      ,#'delete-trailing-whitespace
                      ,#'+jg-lisp-cleanup-ensure-newline
                      ,#'+jg-text-cleanup-whitespace)
    )
(spec-handling-add! lookup-url
            '(lisp
                      ("elisp melpa" "https://melpa.org/#/?q=%s")
                      ("elisp elpa" "https://elpa.gnu.org/packages/")
                      )
                    )
(spec-handling-add! lookup-handler
                    `((emacs-lisp-mode lisp-interaction-mode helpful-mode)
                      :definition    ,#'+emacs-lisp-lookup-definition
                      :documentation ,#'+emacs-lisp-lookup-documentation
                     )
                    '((racket-mode racket-repl-mode)
                     :definition    ,#'+racket-lookup-definition
                     :documentation ,#'+racket-lookup-documentation
                     )
                    '(inferior-emacs-lisp-mode
                      :definition    ,#'+emacs-lisp-lookup-definition
                      :documentation ,#'+emacs-lisp-lookup-documentation
                      )
                    )
(spec-handling-add! ligatures
                    '(emacs-lisp-mode
                      "lambda" ?λ
                      )
                    )
(spec-handling-add! docsets
                    '(racket-mode "Racket")
                    '((emacs-lisp-mode lisp-interaction-mode) "Emacs Lisp")
                    )
(spec-handling-add! evil-embrace
                    `((lisp-mode emacs-lisp-mode clojure-mode racket-mode hy-mode)
                      (?f . ,(make-embrace-pair-struct
                              :key ?f
                              :read-function #'+evil--embrace-elisp-fn
                              :left-regexp "([^ ]+ "
                              :right-regexp ")"))
                      )
                    )
(spec-handling-add! auto-modes
                    '(lisp
                      ("\\.Cask\\'"     . emacs-lisp-mode)
                      ("\\.rkt\\'"      . racket-mode)
                      ("\\.el\\.gz\\'"  . emacs-lisp-mode)
                      ("\\.el\\'"       . emacs-lisp-mode)
                      )
                    )
(spec-handling-add! imenu
                    '(emacs-lisp-mode
                      ("spec-def" "^(spec-handling-new! \\(.+\\) " 1)
                      ("spec-add" "^(spec-handling-add! \\(.+\\) " 1)
                      ("Section" "^[ 	]*;;;*\\**[ 	]+\\([^\n]+\\)" 1)
                      ("Evil commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
                      ("Unit tests" "^\\s-*(\\(?:ert-deftest\\|describe\\) +\"\\([^\")]+\\)\"" 1)
                      ("Package" "^\\s-*\\(?:;;;###package\\|(\\(?:package!\\|use-package!?\\|after!\\)\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
                      ("Major modes" "^\\s-*(define-derived-mode +\\([^ ()\n]+\\)" 1)
                      ("Minor modes" "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
                      ("Modelines" "^\\s-*(def-modeline! +\\([^ ()\n]+\\)" 1)
                      ("Modeline segments" "^\\s-*(def-modeline-segment! +\\([^ ()\n]+\\)" 1)
                      ("Advice" "^\\s-*(\\(?:def\\(?:\\(?:ine-\\)?advice!?\\)\\) +\\([^ )\n]+\\)" 1)
                      ("Macros" "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
                      ("Inline functions" "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
                      ("CLI Command" "^\\s-*(\\(def\\(?:cli\\|alias\\|obsolete\\|autoload\\)! +\\([^\n]+\\)\\)" 1)
                      ("Functions" "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\|-memoized!\\) +\\([^ ,)\n]+\\)" 1)
                      ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var\\(?:-local\\)?\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                      ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                      )
                    )
(spec-handling-add! eval
                    `(emacs-lisp-mode :start ,#'+emacs-lisp/open-repl)
                    `(lisp-interaction-mode :start ,#'+emacs-lisp/open-repl)
                    `(racket-mode :start ,#'+racket/open-repl)
                    )
(spec-handling-add! yas-extra
                    '(buttercup-minor-mode buttercup-minor-mode)
                    )
;;-- end specs
