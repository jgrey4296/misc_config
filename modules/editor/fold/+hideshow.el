;;; +hideshow.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(speckler-new! hideshow (key val)
  "Set hide show special modes"
  :struct '(list (mode "start" "end" "comment-start"...))
  :target hs-special-modes-alist
  :loop 'append
  val
  )

(use-package! hideshow ; built-in
  :commands (hs-toggle-hiding hs-hide-block hs-hide-level hs-show-all hs-hide-all)
  :config
  ;; (advice-add 'hs-toggle-hiding :before #'+fold--hideshow-ensure-mode-a)
  ;; (advice-add 'hs-hide-block :before #'+fold--hideshow-ensure-mode-a)
  ;; (advice-add 'hs-hide-level :before #'+fold--hideshow-ensure-mode-a)
  ;; (advice-add 'hs-show-all :before #'+fold--hideshow-ensure-mode-a)
  ;; (advice-add 'hs-hide-all :before #'+fold--hideshow-ensure-mode-a)

  (defun jg-fold-hs-open-on-move (&rest rest)
    (when (and hs-minor-mode (hs-already-hidden-p))
      (hs-show-block)
      )
    )
  (advice-add 'evil-goto-line :after #'jg-fold-hs-open-on-move)
  )

(setq-default hs-hide-comments-when-hiding-all nil
              hs-set-up-overlay #'+fold-hideshow-set-up-overlay-fn
              )
(speckler-add! hideshow ()
  '(default
    ;;MODE START END COMMENT-START FORWARD-SEXP-FUNC ADJUST-BEG-FUNC
    (vimrc-mode "{{{" "}}}" "\"")
    (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>" "" "#" #'+fold-hideshow-forward-block-by-indent-fn nil)
    (haml-mode "[#.%]" "\n" "/" #'+fold-hideshow-haml-forward-sexp-fn nil)
    (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]" "end\\|[]}]" "#\\|=begin" #'ruby-forward-sexp)
    (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch" "end" nil #'(lambda (_arg) (matlab-forward-sexp)))
    (nxml-mode "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--" sgml-skip-tag-forward nil)
    (latex-mode ;; LaTeX-find-matching-end needs to be inside the env
     ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
     "\\\\end{[a-zA-Z*]+}"
     "%"
     #'(lambda (_arg) ;; Don't fold whole document, that's useless
         (unless (save-excursion
                   (search-backward "\\begin{document}"
                                    (line-beginning-position) t))
           (LaTeX-find-matching-end)))
     nil))
  )

(speckler-add! fold ()
  `(hide-show
    :modes (hs-minor-mode)
    :priority -25
    :triggers (:open-all   #'hs-show-all
               :close-all  #'hs-hide-all
               :toggle     #'hs-toggle-hiding
               :open       #'hs-show-block
               :open-rec   nil
               :close      #'hs-hide-block
               )
    )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 09, 2025
;; Modified:   February 09, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; +hideshow.el ends here
