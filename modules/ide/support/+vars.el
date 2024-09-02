;;; +vars.el -*- lexical-binding: t; -*-
(require 'rx)

;;-- lsp

;; General
(setq lsp-auto-configure                 nil
      lsp-enable-dap-auto-configure      nil

      lsp-enable-file-watchers           nil
      lsp-enable-folding                 nil             ;; can be slow
      lsp-enable-imenu                   nil
      lsp-enable-indentation             nil
      lsp-enable-links                   nil
      lsp-enable-on-type-formatting      nil
      lsp-enable-relative-indentation    nil
      lsp-enable-semantic-highlighting   nil
      lsp-enable-snippet                 nil
      lsp-enable-suggest-server-download nil
      lsp-enable-symbol-highlighting     nil
      lsp-enable-text-document-color     nil             ;; can be slow
      lsp-enable-xref                    t

      lsp-eldoc-enable-hover             nil
      lsp-eldoc-render-all               t
      lsp-completion-enable              t
      lsp-headerline-breadcrumb-enable   nil

      lsp-keep-workspace-alive           nil
      lsp-enabled-clients                nil
      lsp-disabled-clients               nil
      )

;; UI Peek
(setq lsp-ui-peek-enable      t
      lsp-ui-peek-always-show t
      lsp-ui-peek-peek-height 30
      lsp-ui-peek-show-directory nil

)

;; UI Doc
(setq lsp-ui-doc-max-height 15
      lsp-ui-doc-max-width 72         ; 150 (default) is too wide
      lsp-ui-doc-delay 1              ; 0.2 (default) is too naggy
      lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
      lsp-ui-doc-show-with-cursor t
      lsp-ui-doc-position 'bottom
      lsp-ui-doc-include-signature t
      lsp-ui-doc-use-childframe nil
)

;; UI Sideline
(setq lsp-ui-sideline-ignore-duplicate t
      lsp-ui-sideline-delay 1
      lsp-ui-sideline-diagnostic-max-line-length 80
      lsp-ui-sideline-diagnostic-max-lines 3
      lsp-ui-sideline-show-hover       t
      lsp-ui-sideline-actions-icon     nil
)

;; Ignore Directories
(setq lsp-file-watch-ignored-directories
      (list
             (rx "\/" (| "Library" "checkouts" "_FOSSIL_" "_build" "_darcs" "_opam"))
             (rx "\/" (| "autom3te.cache" "bazel-[^/\\]+" "bin/Debug" "build-aux"))
             (rx "\/" (| "dist-newstyle" "dist" "node_modules" "obj" "target" "build" "docs"))
             (rx "\/" (| "data"))

             (rx "\/." (| "temp" "babel_cache" "bloop" "bzr" "ccls-cache"))
             (rx "\/." (| "circleci" "clj-kondo" "cpcache" "deps" "direnv" "elixir_ls"))
             (rx "\/." (| "ensime_cache" "eunit" "fslckout" "git" "github" "gradle" "hg"))
             (rx "\/." (| "idea" "lsp" "m2" "meta" "metals" "mypy_cache" "nox" "reference"))
             (rx "\/." (| "shadow-cljs" "stack-work" "svn" "terraform" "terragrunt-cache"))
             (rx "\/." (| "tox" "venv" "vscode" "yarn"))
             )
      )

;; UI imenu

;;-- end lsp

;;-- eglot
  (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
        ;;      its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil
        eglot-stay-out-of '(flymake)
        )
;;-- end eglot

;;-- flycheck

(defvar flycheck-checkers)

(defvar flycheck-disabled-checkers)

(spec-handling-setq! flycheck 50
                     flycheck-display-errors-delay 1
                     flycheck-display-errors-function nil
                     flycheck-help-echo-function nil
                     flycheck-process-error-functions nil
                     flycheck-check-syntax-automatically '(save idle-change mode-enabled)
                     flycheck-idle-change-delay 1.0
                     flycheck-buffer-switch-check-intermediate-buffers t
                     flycheck-display-errors-delay 0.25
                     flycheck-popup-tip-error-prefix "X "
                     flycheck-posframe-warning-prefix "! "
                     flycheck-posframe-info-prefix "··· "
                     flycheck-posframe-error-prefix "X "
                     flycheck-indication-mode 'right-fringe
                     )

;;-- end flycheck

;;-- tree-sitter
(setq tree-sitter-debug-jump-buttons t ;; This makes every node a link to a section of code
      tree-sitter-debug-highlight-jump-region t ;; and this highlights the entire sub tree in your code
      ;; MAYBE make this a spec-handler:
      tree-sitter-load-path (list
                             (expand-file-name (format "straight/%s/tree-sitter-langs/bin/" straight-build-dir) doom-local-dir)
                             (expand-file-name "~/.local/tree-sitter/")
                             )

      )

(setq treesit-extra-load-path tree-sitter-load-path
      ;; treesit-simple-indent-rules
      ;; treesit-defun-type-regexp
      ;; treesit-defun-name-function
      ;; treesit-simple-imenu-settings
      ;; treesit-max-buffer-size
      ;;

      ;; TODO make this a spec handler
      treesit-language-source-alist '(
                                      ;; expects (lang . (url revision source-dir CC C++))
                                      (elisp         "git@github.com:wilfred/tree-sitter-elisp.git")
                                      (python        "git@github.com:tree-sitter/tree-sitter-python.git")
                                      (toml          "git@github.com:ikatyang/tree-sitter-toml.git")
                                      (bash          "git@github.com:tree-sitter/tree-sitter-bash.git")
                                      (bibtex        "git@github.com:latex-lsp/tree-sitter-bibtex.git")
                                      (csharp        "git@github.com:tree-sitter/tree-sitter-c-sharp.git")
                                      (css           "git@github.com:tree-sitter/tree-sitter-css.git")
                                      (csv           "git@github.com:tree-sitter-grammars/tree-sitter-csv.git")
                                      (elixir        "git@github.com:elixir-lang/tree-sitter-elixir.git")
                                      (erlang        "git@github.com:WhatsApp/tree-sitter-erlang.git")
                                      (gdscript      "git@github.com:PrestonKnopp/tree-sitter-gdscript.git")
                                      (gitattributes "git@github.com:tree-sitter-grammars/tree-sitter-gitattributes.git")
                                      (gitignore     "git@github.com:shunsambongi/tree-sitter-gitignore.git")
                                      (glsl          "git@github.com:tree-sitter-grammars/tree-sitter-glsl.git")
                                      (groovy        "git@github.com:Decodetalkers/tree-sitter-groovy.git")
                                      (haskell       "git@github.com:tree-sitter/tree-sitter-haskell.git")
                                      (java          "git@github.com:tree-sitter/tree-sitter-java.git")
                                      (json          "git@github.com:tree-sitter/tree-sitter-json.git")
                                      (kotlin        "git@github.com:fwcd/tree-sitter-kotlin.git")
                                      (latex         "git@github.com:latex-lsp/tree-sitter-latex.git")
                                      (llvm          "git@github.com:benwilliamgraham/tree-sitter-llvm.git")
                                      (llvm-mir      "git@github.com:Flakebi/tree-sitter-llvm-mir.git")
                                      (markdown      "git@github.com:tree-sitter-grammars/tree-sitter-markdown.git")
                                      (ocaml         "git@github.com:tree-sitter/tree-sitter-ocaml.git")
                                      (org           "git@github.com:milisims/tree-sitter-org.git")
                                      (rst           "git@github.com:stsewd/tree-sitter-rst.git")
                                      (rust          "git@github.com:tree-sitter/tree-sitter-rust.git")
                                      (sql           "git@github.com:DerekStride/tree-sitter-sql.git")
                                      (xml           "git@github.com:tree-sitter-grammars/tree-sitter-xml.git")
                                      (yaml          "git@github.com:ikatyang/tree-sitter-yaml.git")
                                      )
 )

(spec-handling-setq! treesit 50
                     tree-sitter-load-path (list
                                            (expand-file-name (format "straight/%s/tree-sitter-langs/bin/" straight-build-dir) doom-local-dir)
                                            (expand-file-name "~/.local/tree-sitter/")
                                            )
                     treesit-extra-load-path tree-sitter-load-path
                     )

;;todo: use treesit-font-lock-rules
;;-- end tree-sitter

;;-- specs
(spec-handling-add! popup
                    '(lsp
                      ("^\*lsp session\*"  :side right  :ttl nil :width 0.5 :quit t :select nil :priority 50)
                      ("^\\*lsp-\\(help\\|install\\)" :size 0.35 :quit t :select t)
                      ("^\\*eglot-help" :size 0.15 :quit t :select t)
                     )
                    '(flycheck
                      ("^\\*Flycheck error messages\\*" :select nil)
                      ("^\\*Flycheck errors\\*" :size 0.25)
                      )
                    )

(spec-handling-add! fold
                    `(lsp-browser
                     :modes (lsp-browser-mode)
                     :priority 30
                     :triggers (:open-all   ,#'+jg-lsp-toggle-widget-on-line
                                :close-all  ,#'+jg-lsp-toggle-widget-on-line
                                :toggle     ,#'+jg-lsp-toggle-widget-on-line
                                :open       ,#'+jg-lsp-toggle-widget-on-line
                                :open-rec   ,#'+jg-lsp-toggle-widget-on-line
                                :close      ,#'+jg-lsp-toggle-widget-on-line
                                )
                     )
                    )

(spec-handling-add! lookup-handler
                    `(lsp-mode
                     :definition          +lsp-lookup-definition-handler
                     :declaration         lsp-find-declaration
                     :references          +lsp-lookup-references-handler
                     :documentation       lsp-describe-thing-at-point
                     :implementations     lsp-find-implementation
                     :type-definition     lsp-find-type-definition
                     )
                    `(eglot--managed-mode
                     :definition          xref-find-definitions
                     :references          xref-find-references
                     :implementations     eglot-find-implementation
                     :type-definition     eglot-find-typeDefinition
                     :documentation       +eglot-lookup-documentation
                     )
                    `(lsp-ui-mode
                      :definition         lsp-ui-peek-find-definitions
                      :implementations    lsp-ui-peek-find-implementation
                      :references         lsp-ui-peek-find-references
                      :async t
                      )
                    )

(spec-handling-add! tree-sit-lang
                    '(agda-mode       . agda)
                    '(c-mode          . c)
                    '(c++-mode        . cpp)

                    '(elm-mode        . elm)

                    '(julia-mode      . julia)
                    '(ruby-mode       . ruby)
                    '(tuareg-mode     . ocaml)
                    )

(spec-handling-add! env-handling
                    '(flycheck
                      (:support flycheck #'(lambda (path name)
                                             (when (featurep 'flycheck)
                                               (unless flycheck-enabled-checkers
                                                 (let ((chosen (intern (ivy-read "Flychecker: " flycheck-disabled-checkers :require-match t))))
                                                   (delete chosen flycheck-disabled-checkers)
                                                   (add-to-list flycheck-enabled-checkers chosen)
                                                   ))
                                               (add-hook 'python-mode-hook #'flycheck-mode)
                                               )
                                             )
                                (-partial #'flycheck-mode -1)
                                )
                      (:teardown flycheck (-partial flycheck-mode -1))
                      )
                      )

(spec-handling-add! env-handling
                    '(lsp
                      (:support lsp
                                #'(lambda (state) (when (featurep 'lsp) (add-hook 'python-mode-hook #'lsp-deferred)))
                                #'(lambda (state)
                                    (when (featurep 'lsp)
                                      (when lsp-mode (lsp-mode -1))
                                      (when lsp--last-active-workspaces
                                        (lsp-workspace-shutdown (car lsp--last-active-workspaces)))
                                      (remove-hook 'python-mode-hook #'lsp-deferred)
                                      )
                                     )
                                )
                      (:teardown lsp #'(lambda (state) (when (featurep 'lsp) (lsp-disconnect))))
                      )
                    )

(spec-handling-add! env-handling
                    '(eglot
                      (:support eglot
                                #'(lambda (state) (when (featurep 'eglot) (add-hook 'python-mode-hook #'eglot-ensure)))
                                #'(lambda (state)
                                    (when (featurep 'eglot) (remove-hook 'python-mode-hook #'eglot-ensure)))
                                )
                      )
                    )

(spec-handling-add! env-handling
                    '(semantic
                      (:support semantic
                                #'(lambda (state) (when (featurep 'semantic) (add-hook 'python-mode-hook #'semantic-mode)))
                                #'(lambda (state) (when (featurep 'semantic) (remove-hook 'python-mode-hook #'semantic-mode)))
                                )
                      )
                    )
;;-- end specs
