;;; +lsp.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'rx)

(use-package! lsp-mode
  :commands (lsp-install-server lsp-deferred lsp-update-servers)
  :init
  ;; Don't touch ~/.emacs.d, which could be purged without warning
  (setq lsp-session-file (concat doom-cache-dir "lsp-session")
        lsp-server-install-dir (concat doom-data-dir "lsp")
        lsp-keymap-prefix nil)

  ;; override what is auto loaded
  (setq lsp-client-packages nil)

  :config
  (add-to-list 'doom-debug-variables 'lsp-log-io)

  (setq lsp-xml-jar-file (expand-file-name "org.eclipse.lsp4xml-0.3.0-uber.jar" lsp-server-install-dir)
        lsp-groovy-server-file (expand-file-name "groovy-language-server-all.jar" lsp-server-install-dir))

  (add-hook! 'lsp-mode-hook #'+lsp-optimization-mode)

)

(use-package! lsp-ui
  :commands (lsp-ui-doc-mode lsp-ui-imenu lsp-ui-sideline)
  )

(use-package! lsp-ivy
  :when (modulep! :ui ivy)
  :commands lsp-ivy--transform-candidate
  )

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

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 10, 2024
;; Modified:   September 10, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +lsp.el ends here
