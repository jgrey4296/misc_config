;;; +treesitter.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! tree-sitter ;; Melpas
  :disabled t
  :config
  (require 'tree-sitter-langs)
  )

(use-package! treesit ;; builtin
  :defer t
  :config
  (require 'tree-sitter-langs)
  ;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
  )

(use-package! tree-sitter-langs
  :defer t
  :config
  ;; possibly: (cl-pushnew (tree-sitter-lands--bin-dir) tree-sitter-load-path :test #'string-equal)
  )

(speckler-new! tree-sit-lang
                    "Match modes to grammars in `tree-sitter-langs-grammar-dir`"
                    :target tree-sitter-major-mode-language-alist
                    :struct '(key-mode . grammar)
                    :loop 'collect
                    `(,key . ,val)
                    )

(speckler-new! treesit-lang
                    "for treesit (builtin)"
                    :target treesit-load-name-override-list
                    :struct '(key-mode :lib-base :entry-func)
                    :loop 'collect
                    `(,key ,(plist-get val :lib-base) ,(plist-get val :entry-func))
                    )

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

(speckler-setq! treesit 50
                     tree-sitter-load-path (list
                                            (expand-file-name (format "straight/%s/tree-sitter-langs/bin/" straight-build-dir) doom-local-dir)
                                            (expand-file-name "~/.local/tree-sitter/")
                                            )
                     treesit-extra-load-path tree-sitter-load-path
                     )

(speckler-add! tree-sit-lang
                    '(agda-mode       . agda)
                    '(c-mode          . c)
                    '(c++-mode        . cpp)

                    '(elm-mode        . elm)

                    '(julia-mode      . julia)
                    '(ruby-mode       . ruby)
                    '(tuareg-mode     . ocaml)
                    )

;;todo: use treesit-font-lock-rules
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
;;; +treesitter.el ends here
