;;; +vars.el -*- lexical-binding: t; -*-

;;-- definitions

(defvar +lsp-defer-shutdown 3 "If non-nil, defer shutdown of LSP servers for this many seconds after last workspace buffer is closed.")

(defvar +lsp-company-backends
  (if (modulep! :editor snippets)
      '(:separate company-capf company-yasnippet)
    'company-capf)
  "The backends to prepend to `company-backends' in `lsp-mode' buffers.
Can be a list of backends; accepts any value `company-backends' accepts.")

(defvar +lsp--default-read-process-output-max nil)

(defvar +lsp--default-gcmh-high-cons-threshold nil)

(defvar +lsp--optimization-init-p nil)

(defvar +lsp--deferred-shutdown-timer nil)

;;-- end definitions

;;-- lsp
(setq lsp-ui-peek-enable nil
      lsp-ui-doc-max-height 8
      lsp-ui-doc-max-width 72         ; 150 (default) is too wide
      lsp-ui-doc-delay 1              ; 0.2 (default) is too naggy
      lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
      lsp-ui-doc-position 'at-point
      lsp-ui-sideline-ignore-duplicate t
      lsp-ui-sideline-show-hover nil
      lsp-ui-sideline-actions-icon nil
      lsp-keep-workspace-alive nil
      lsp-enable-folding nil             ;; can be slow
      lsp-enable-text-document-color nil ;; ditto
      lsp-enable-on-type-formatting nil
      lsp-headerline-breadcrumb-enable nil
      )

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

;;-- ignore dirs
(setq lsp-file-watch-ignored-directories (rx "\/" (|
                                                   "Library" "checkouts" "_FOSSIL_" "_build" "_darcs" "_opam" "autom4te.cache" "bazel-[^/\\]+" "bin/Debug" "build-aux" "dist-newstyle" "dist" "node_modules" "obj" "target" "build" "docs" "data"
                                                   (: "." (| "temp" "babel_cache" "bloop" "bzr" "ccls-cache" "circleci" "clj-kondo" "cpcache" "deps" "direnv" "elixir_ls" "ensime_cache" "eunit" "fslckout" "git" "github" "gradle" "hg" "idea" "lsp" "m2" "meta" "metals" "mypy_cache" "nox" "reference" "shadow-cljs" "stack-work" "svn" "terraform" "terragrunt-cache" "tox" "venv" "vscode" "yarn"))
                                                   )
                                             eos
                                             )
      )

;;-- end ignore dirs

;;-- specs
(spec-handling-add! popup nil
                    ('lsp
                     (
                      ("^\*lsp session\*"  :side right  :ttl nil :width 0.5 :quit t :select nil :priority 50)
                      ("^\\*lsp-\\(help\\|install\\)" :size 0.35 :quit t :select t)
                      ("^\\*eglot-help" :size 0.15 :quit t :select t)
                      )
                     )
                    )

(spec-handling-add! fold nil
                    ('lsp-browser
                     :modes (lsp-browser-mode)
                     :priority 30
                     :triggers (:open-all   +jg-lsp-toggle-widget-on-line
                                :close-all  +jg-lsp-toggle-widget-on-line
                                :toggle     +jg-lsp-toggle-widget-on-line
                                :open       +jg-lsp-toggle-widget-on-line
                                :open-rec   +jg-lsp-toggle-widget-on-line
                                :close      +jg-lsp-toggle-widget-on-line
                                )
                     )
                    )

;; (spec-handling-add! lookup-url nil
;;                     ('lsp
;;                      ("lsp" "url")
;;                      )
;;                     )

(spec-handling-add! lookup-handler nil
                    (lsp-mode
                     :definition #'+lsp-lookup-definition-handler
                     :references #'+lsp-lookup-references-handler
                     :documentation '(lsp-describe-thing-at-point :async t)
                     :implementations '(lsp-find-implementation :async t)
                     :type-definition #'lsp-find-type-definition
                     )
                    (eglot--managed-mode
                     :definition      #'xref-find-definitions
                     :references      #'xref-find-references
                     :implementations #'eglot-find-implementation
                     :type-definition #'eglot-find-typeDefinition
                     :documentation   #'+eglot-lookup-documentation
                     )
                    )

(when (modulep! +peek)
  (spec-handling-add! lookup-handler nil
                      (lsp-ui-mode
                       :definition         'lsp-ui-peek-find-definitions
                       :implementations    'lsp-ui-peek-find-implementation
                       :references         'lsp-ui-peek-find-references
                       :async t
                       )
                      )
  )



;;-- end specs
