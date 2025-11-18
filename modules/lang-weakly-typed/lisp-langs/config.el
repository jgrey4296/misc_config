;;; lang/emacs-lisp/config.el -*- lexical-binding: t; no-byte-compile: t -*-
(declare-function local-load! "defer-macro")
(declare-function defer-load! "defer-macro")

(local-load! "+defs")
(local-load! "+extra")
(local-load! "+flycheck")
(local-load! "+emacs")
(local-load! "+testing")
(when (modulep! +racket) (local-load! "+racket"))

(defer-load! jg-bindings-total "+bindings")

;; --------------------------------------------------

(after! projectile
  (add-to-list 'projectile-project-root-files "config.el")
  (add-to-list 'projectile-project-root-files "info.rkt")
  )

(after! smartparens
  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))
  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))
  )

;; --------------------------------------------------

(speckler-add! popup ()
  '(lisp
    ("^\\*Buttercup\\*'" :size 0.45 :select nil :ttl 0)
    ("^*ert*" :width 0.4 :side right :select nil :ttl 0 )
    )
  )
(speckler-add! fold ()
  :override nil
  `(lisp
    :modes (emacs-lisp-mode lisp-mode)
    :priority 125
    :triggers (:open-all  #'hs-show-all
               :close-all #'hs-hide-all
               :toggle    #'hs-toggle-hiding
               :open      #'hs-show-block
               :open-rec  nil
               :close     #'hs-hide-block
               )
    )
  )
(speckler-add! file-templates ()
  :override t
  `(lisp
    ("minor-mode\\.el\\'" :trigger "__minor-mode" :mode emacs-lisp-mode)
    ("mode\\.el\\'"       :trigger "__mode"       :mode emacs-lisp-mode)
    ("ob-.+?\\.el\\'"     :mode emacs-lisp-mode :trigger "__org_babel")
    ("/.dir-locals.el\\'" :mode emacs-lisp-mode :trigger "__dir_locals")
    ("-tests?\\.el\\'"    :mode emacs-lisp-mode :trigger "__test")
    ("--tests?-.+?\\.el"  :mode emacs-lisp-mode :trigger "__test")
    (emacs-lisp-mode      :trigger "__package" :priority -199)
    )
  )
(speckler-add! projects ()
  '(emacs-eldev projectile-eldev-project-p :project-file "eldev" :compilation-dir nil :configure nil :compile "eldev compile" :test "eldev test" :install nil :package "eldev package" :run "eldev emacs")
  '(emacs-cask ("cask") :project-file "cask" :compilation-dir nil :configure nil :compile "cask install" :test nil :install nil :package nil :run nil :test-suffix "-test" :test-prefix "test-")
  )
(speckler-add! rotate-text ()
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
              ("car" "cdr" "cadr" "caddr")
              )
    )
  '(racket-mode
    :symbols (("#true" "#false"))
    )
  )
(speckler-add! whitespace-cleanup ()
  '(emacs-lisp-mode
    #'delete-trailing-whitespace
    #'+jg-lisp-cleanup-ensure-newline
    #'+jg-text-cleanup-whitespace)
  )
(speckler-add! online-search ()
  '(lisp
    ("elisp melpa" "https://melpa.org/#/?q=%s")
    ("elisp elpa" "https://elpa.gnu.org/packages/")
    )
  )
(speckler-add! doc-lookup ()
  `((emacs-lisp-mode lisp-interaction-mode helpful-mode)
    :definition     #'elisp-def
    :documentation  #'helpful-at-point
    )
  '((racket-mode racket-repl-mode)
    :definition    #'+racket-lookup-definition
    :documentation #'+racket-lookup-documentation
    )
  '(inferior-emacs-lisp-mode
    :definition    #'+emacs-lisp-lookup-definition
    :documentation #'+emacs-lisp-lookup-documentation
    )
  )
(speckler-add! ligatures ()
  '(emacs-lisp-mode
    "lambda" ?λ
    )
  )
(speckler-add! docsets ()
  '(racket-mode "Racket")
  '((emacs-lisp-mode lisp-interaction-mode) "Emacs Lisp")
  )
(speckler-add! evil-embrace ()
  `((lisp-mode emacs-lisp-mode clojure-mode racket-mode hy-mode)
    (?f . ,(make-embrace-pair-struct
            :key ?f
            :read-function #'+evil--embrace-elisp-fn
            :left-regexp "([^ ]+ "
            :right-regexp ")"))
    )
  )
(speckler-add! auto-modes ()
  '(lisp
    ("\\.Cask\\'"     . emacs-lisp-mode)
    ("\\.rkt\\'"      . racket-mode)
    ("\\.el\\.gz\\'"  . emacs-lisp-mode)
    ("\\.el\\'"       . emacs-lisp-mode)
    )
  )
(speckler-add! imenu ()
  :override nil
  '(emacs-lisp-mode
    :append
    ("spec-def"             "^(speckler-new! \\(.+\\)" 1)
    ("spec-hook"            "^(speckler-new-hook! \\(.+?\\)" 1)
    ("spec-add"             "^(speckler-add! \\(.+\\) " 1)
    ("Section"              "^[ 	]*;;;*\\**[ 	]+\\([^\n]+\\)" 1)
    ("Evil commands"        "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
    ("Unit tests"           "^\\s-*(\\(?:ert-deftest\\|describe\\) +\"\\([^\")]+\\)\"" 1)
    ("Package"              "^\\s-*\\(?:;;;###package\\|(\\(?:package!\\|use-package!?\\|after!\\)\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
    ("Major modes"          "^\\s-*(define-derived-mode +\\([^ ()\n]+\\)" 1)
    ("Minor modes"          "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
    ("Modelines"            "^\\s-*(def-modeline! +\\([^ ()\n]+\\)" 1)
    ("Modeline segments"    "^\\s-*(def-modeline-segment! +\\([^ ()\n]+\\)" 1)
    ("Advice"               "^\\s-*(\\(?:def\\(?:\\(?:ine-\\)?advice!?\\)\\) +\\([^ )\n]+\\)" 1)
    ("Macros"               "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
    ("Inline functions"     "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
    ("CLI Command"          "^\\s-*(\\(def\\(?:cli\\|alias\\|obsolete\\|autoload\\)! +\\([^\n]+\\)\\)" 1)
    ("Functions"            "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\|-memoized!\\) +\\([^ ,)\n]+\\)" 1)
    ("Variables"            "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var\\(?:-local\\)?\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
    ("Types"                "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
    )
  )
(speckler-add! repl ()
  '(emacs-lisp-mode       :start +emacs-lisp/open-repl :send +jg-lisp-eval)
  '(lisp-interaction-mode :start +emacs-lisp/open-repl :send +jg-lisp-eval)
  )
(speckler-add! yas-extra ()
  '(buttercup-minor-mode buttercup-minor-mode)
  )
(speckler-add! eval ()
  '(elisp-mode :fn eval-region)
  )
(speckler-add! org-src ()
  '(lisp
    ("elisp" . emacs-lisp)
    )
  )
(speckler-add! babel ()
  '(lisp
    (:name lisp       :lib ob-lisp)
    (:name elisp      :lib ob-emacs-lisp)
    (:name clojure    :lib ob-clojure)
    (:name scheme     :lib ob-scheme)
    (:name emacs-lisp :lib ob-emacs-lisp)
    )
  )
(speckler-add! treesit-source ()
  '(elisp         "git@github.com:wilfred/tree-sitter-elisp.git")
  )
