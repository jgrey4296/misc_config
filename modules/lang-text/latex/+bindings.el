;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-latex-mode-map
      :n "\"" #'TeX-insert-quote
      :i "\"" (cmd! (insert "\""))
      :i "$"  #'TeX-insert-dollar
      :n "RET" #'+jg-latex-compile-file-quick

      :localleader
      :desc "Preview"       "p" #'TeX-command-master
      :desc "TexDoc"        "?" #'TeX-documentation-texdoc
      :desc "section"        "s" #'LaTeX-section
      :desc "Font Change"   "f" #'TeX-font

      (:prefix ("i" . "insert")
       :desc "item"          "i" #'LaTeX-insert-item
       :desc "macro"         "m" #'TeX-insert-macro
       :desc "block"         "b" #'latex-insert-block
       :desc "function"      "f" #'helm-insert-latex-math
       :desc "environment"   "e" #'LaTeX-environment
       :desc "close env"     "E" #'LaTeX-close-environment
       :desc "Package"       "p" #'+jg-latex-insert-package
       )
      )

(map! :map reftex-mode-map
      :localleader
      ";" 'reftex-toc)

;; Disabling keys that have overlapping functionality with other parts of Doom.
(map! :map cdlatex-mode-map
      ;; Smartparens takes care of inserting closing delimiters, and if you
      ;; don't use smartparens you probably don't want these either.
      "$" nil "(" nil "{" nil "[" nil "|" nil "<" nil
      ;; TAB is used for CDLaTeX's snippets and navigation. But we have
      ;; Yasnippet for that.
      "TAB nil"
      ;; AUCTeX takes care of auto-inserting {} on _^ if you want, with
      ;; `TeX-electric-sub-and-superscript'.
      "^" nil "_" nil
      ;; AUCTeX already provides this with `LaTeX-insert-item'.
        [(control return)] nil)

(map! :map dired-mode-map
      :localleader
      "g t" #'+jg-latex-dired-build-font-examples
      )

(after! latex
  (setq LaTeX-mode-map jg-latex-mode-map)
  )
(after! tex-fold
  (setq TeX-fold-mode-map (make-sparse-keymap)
        TeX-fold-keymap   (make-sparse-keymap)
        )
  )
