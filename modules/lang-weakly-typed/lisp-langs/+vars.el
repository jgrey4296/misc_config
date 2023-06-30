;;; +vars.el -*- lexical-binding: t; -*-

;;-- definitions

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and variables.")

(defvar +emacs-lisp-outline-regexp "[ \t]*;;;\\(;*\\**\\) [^ \t\n]"
  "Regexp to use for `outline-regexp' in `emacs-lisp-mode'.
This marks a foldable marker for `outline-minor-mode' in elisp buffers.")

(defvar +emacs-lisp-linter-warnings
  '(not free-vars    ; don't complain about unknown variables
        noruntime    ; don't complain about unknown function calls
        unresolved)  ; don't complain about undefined functions
  "The value for `byte-compile-warnings' in non-packages.

This reduces the verbosity of flycheck in Emacs configs and scripts, which are
so stateful that the deluge of false positives (from the byte-compiler,
package-lint, and checkdoc) can be more overwhelming than helpful.

See `+emacs-lisp-non-package-mode' for details.")

;;-- end definitions

(after! projectile
  (pushnew! projectile-project-root-files "config.el")
  )

;;-- emacs source paths
(after! (ffap find-func)
  (let ((paths-to-add (append
                       (ffap-all-subdirs "/Volumes/documents/github/_libs/lisp/emacs-src/lisp/" 1)
                       (ffap-all-subdirs (expand-file-name "straight/repos" doom-local-dir) 1)
                       (ffap-all-subdirs (expand-file-name "modules" doom-user-dir))
                       (ffap-all-subdirs (expand-file-name "packages" doom-user-dir))
                       )))
  (mapc (lambda (x)
          (add-to-list 'find-library-source-path x))
        paths-to-add)
  )
  (setq find-function-C-source-directory "/Volumes/documents/github/_libs/lisp/emacs-src/src")

)
;;-- end emacs source paths

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
                     ("/test/.+\\.el$"   :when ,#'+file-templates-in-emacs-dirs-p :trigger "__doom-test"     :mode emacs-lisp-mode)
                     ("/doctor\\.el$"    :when ,#'+file-templates-in-emacs-dirs-p :trigger "__doom-doctor"   :mode emacs-lisp-mode)
                     ("config\\.el$"     :when ,#'+file-templates-in-emacs-dirs-p :trigger "__doom_config"   :mode emacs-lisp-mode)
                     ("packages\\.el$"   :when ,#'+file-templates-in-emacs-dirs-p :trigger "__doom_packages" :mode emacs-lisp-mode)
                     ("minor-mode\\.el$" :trigger "__minor-mode" :mode emacs-lisp-mode)
                     ("mode\\.el$"       :trigger "__mode"       :mode emacs-lisp-mode)
                     ("ob-.+?\\.el$"     :mode emacs-lisp-mode :trigger "__org_babel")
                     ("/.dir-locals.el$" :mode emacs-lisp-mode :trigger "__dir_locals")
                     ("-test\\.el$"      :mode emacs-ert-mode)
                     ("\\.el$"           :when ,#'+file-templates-in-emacs-dirs-p :trigger "__doom-module" :mode emacs-lisp-mode)
                     (emacs-lisp-mode    :trigger "__package")
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

(set-repl-handler! '(emacs-lisp-mode lisp-interaction-mode) #'+emacs-lisp/open-repl)
(set-repl-handler! 'racket-mode #'+racket/open-repl)
(set-eval-handler! '(emacs-lisp-mode lisp-interaction-mode) #'+emacs-lisp-eval)
(set-eval-handler! '(emacs-lisp-mode lisp-interaction-mode) #'+jg-lisp-eval)

;;-- end specs

(spec-handling-add! lookup-regular
                    '(emacs-lisp-mode
                      ("Common Lisp"      . "https://www.gnu.org/software/emacs/manual/html_node/cl/index.html")
                      ("Doom"             . "https://github.com/doomemacs/doomemacs")
                      ("Evil Guide"       . "https://github.com/noctuid/evil-guide")
                      ("Evil docs"        . "https://evil.readthedocs.io/en/latest/settings.html")
                      ("Flycheck"         . "https://www.flycheck.org/en/latest/")
                      ("GNU Lisp"         . "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html")
                      ("Melpa"            . "https://melpa.org/#/")
                      ("Origami"          . "https://github.com/gregsexton/origami.el")
                      ("Yasnippet Manual" . "https://joaotavora.github.io/yasnippet/snippet-development.html")
                      ("dash"             . "https://github.com/magnars/dash.el")
                      ("evil-mc"          . "https://github.com/gabesoft/evil-mc")
                      ("multiple-cursors" . "https://github.com/magnars/multiple-cursors.el")
                      ("s"                . "https://github.com/magnars/s.el")
                      ("transient"        . "https://magit.vc/manual/transient/")
                      )
                    '(racket-mode
                      ("Racket Docs" . "https://docs.racket-lang.org/")
                      )
                    )
