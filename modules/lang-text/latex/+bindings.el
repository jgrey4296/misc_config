;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map latex-mode-map
      :localleader
      :desc "View"          "v" #'TeX-view
      :desc "Compile"       "c" #'TeX-command-run-all
      :desc "Run a command" "m" #'TeX-command-master)

(map! :map LaTeX-mode-map
      :localleader
      :desc "Preview"       "p" #'preview-at-point
      :desc "Unpreview"     "P" #'preview-clearout-at-point
      :desc "View"          "v" #'TeX-view
      :desc "Compile"       "c" #'TeX-command-run-all
      :desc "Run a command" "m" #'TeX-command-master)

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
