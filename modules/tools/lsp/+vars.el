;;; +vars.el -*- lexical-binding: t; -*-

(defvar +lsp-defer-shutdown 3
  "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.

This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer, or when programmatically
killing and opening many LSP/eglot-powered buffers.")

(defvar +lsp-company-backends
  (if (modulep! :editor snippets)
      '(:separate company-capf company-yasnippet)
    'company-capf)
  "The backends to prepend to `company-backends' in `lsp-mode' buffers.
Can be a list of backends; accepts any value `company-backends' accepts.")

;;
;;; Common

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)

(setq lsp-file-watch-ignored-directories (rx "\/" (|
                                                   "Library"
                                                   "checkouts"
                                                   "_FOSSIL_"
                                                   "_build"
                                                   "_darcs"
                                                   "_opam"
                                                   "autom4te.cache"
                                                   "bazel-[^/\\]+"
                                                   "bin/Debug"
                                                   "build-aux"
                                                   "dist-newstyle"
                                                   "dist"
                                                   "node_modules"
                                                   "obj"
                                                   "target"
                                                   "build"
                                                   "docs"
                                                   "data"
                                                   (: "."
                                                      (|
                                                       "temp"
                                                       "babel_cache"
                                                       "bloop"
                                                       "bzr"
                                                       "ccls-cache"
                                                       "circleci"
                                                       "clj-kondo"
                                                       "cpcache"
                                                       "deps"
                                                       "direnv"
                                                       "elixir_ls"
                                                       "ensime_cache"
                                                       "eunit"
                                                       "fslckout"
                                                       "git"
                                                       "github"
                                                       "gradle"
                                                       "hg"
                                                       "idea"
                                                       "lsp"
                                                       "m2"
                                                       "meta"
                                                       "metals"
                                                       "mypy_cache"
                                                       "nox"
                                                       "reference"
                                                       "shadow-cljs"
                                                       "stack-work"
                                                       "svn"
                                                       "terraform"
                                                       "terragrunt-cache"
                                                       "tox"
                                                       "venv"
                                                       "vscode"
                                                       "yarn"
                                                       )
                                                      )
                                                   )
                                             eos
                                             )
      )

;;-- popup
(after! jg-ui-reapply-hook-ready
  (+jg-popup-add-spec
   'lsp
   '(
     ("^\*lsp session\*"  :side right  :ttl nil :width 0.5 :quit t :select nil :priority 50)
     )
   )
  )
;;-- end popup

;;-- fold spec
(after! jg-ui-reapply-hook-ready
  (+jg-fold-add-spec 'lsp-browser
                     `((lsp-browser-mode)
     :open-all   +jg-lsp-toggle-widget-on-line
     :close-all  +jg-lsp-toggle-widget-on-line
     :toggle     +jg-lsp-toggle-widget-on-line
     :open       +jg-lsp-toggle-widget-on-line
     :open-rec   +jg-lsp-toggle-widget-on-line
     :close      +jg-lsp-toggle-widget-on-line
     ))
  )
;;-- end fold spec
