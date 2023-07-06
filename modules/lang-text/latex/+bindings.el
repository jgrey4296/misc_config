;;; +bindings.el -*- lexical-binding: t; -*-

(defvar jg-latex-mode-map (make-sparse-keymap))

(map! :map LaTeX-mode-map
      :i "\"" #'TeX-insert-quote
      :i "$"  #'TeX-insert-dollar
      :n "|"  #'+jg-ivy-general-insert
      :n "RET" #'+jg-latex-compile-file-quick

      :localleader
      :desc "Preview"       "p" #'preview-at-point
      :desc "Unpreview"     "P" #'preview-clearout-at-point
      :desc "View"          "v" #'TeX-view
      :desc "Compile"       "c" #'TeX-command-run-all
      :desc "Run a command" "m" #'TeX-command-master
      :desc "TexDoc"        "?" #'TeX-documentation-texdoc
      :desc "environment"   "e" #'LaTeX-environment
      :desc "close environment" "E" #'LaTeX-close-environment
      :desc "section"        "s" #'LaTeX-section
      :desc "item"          "i" #'LaTeX-insert-item
      :desc "Font Change"   "f" #'TeX-font

      )

(map! :map TeX-fold-mode-map
      :localleader
      :desc "Fold paragraph"   "f"   #'TeX-fold-paragraph
      :desc "Unfold paragraph" "F"   #'TeX-fold-clearout-paragraph
      :desc "Unfold buffer"    "C-f" #'TeX-fold-clearout-buffer)

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

(after! latex
  (setq LaTeX-mode-map jg-latex-mode-map)
  )
