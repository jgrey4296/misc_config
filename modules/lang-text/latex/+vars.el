;;; +vars.el -*- lexical-binding: t; -*-

;;-- module defvars
(defconst +latex-indent-item-continuation-offset 'align
  "Level to indent continuation of enumeration-type environments.

I.e., this affects \\item, \\enumerate, and \\description.

Set this to `align' for:

  \\item lines aligned
         like this.

Set to `auto' for continuation lines to be offset by `LaTeX-indent-line':

  \\item lines aligned
    like this, assuming `LaTeX-indent-line' == 2

Any other fixed integer will be added to `LaTeX-item-indent' and the current
indentation level.

Set this to `nil' to disable all this behavior.

You'll need to adjust `LaTeX-item-indent' to control indentation of \\item
itself.")

(defvar +latex-enable-unicode-math nil
  "If non-nil, use `company-math-symbols-unicode' backend in `LaTeX-mode',
enabling unicode symbols in math regions. This requires the unicode-math LaTeX
package to be installed.")

(defvar +latex-viewers '(skim evince sumatrapdf zathura okular pdf-tools)
  "A list of enabled LaTeX viewers to use, in this order. If they don't exist,
they will be ignored. Recognized viewers are skim, evince, sumatrapdf, zathura,
okular and pdf-tools.

If no viewer is found, `latex-preview-pane-mode' is used.")

;;-- end module defvars

(setq-default TeX-master t)
(setq TeX-parse-self t ; parse on load
      TeX-auto-save t  ; parse on save
      ;; Use hidden directories for AUCTeX files.
      TeX-auto-local ".auctex-auto"
      TeX-style-local ".auctex-style"
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      ;; Don't start the Emacs server when correlating sources.
      TeX-source-correlate-start-server nil
      ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
      TeX-electric-sub-and-superscript t
      ;; Just save, don't ask before each compilation.
      TeX-save-query nil
      LaTeX-enable-toolbar nil
      )

;;-- reftex
;; Get RefTeX working with BibLaTeX, see
;; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992.
(setq reftex-cite-format
      '((?a . "\\autocite[]{%l}")
        (?b . "\\blockcquote[]{%l}{}")
        (?c . "\\cite[]{%l}")
        (?f . "\\footcite[]{%l}")
        (?n . "\\nocite{%l}")
        (?p . "\\parencite[]{%l}")
        (?s . "\\smartcite[]{%l}")
        (?t . "\\textcite[]{%l}"))
      reftex-plug-into-AUCTeX t
      reftex-toc-split-windows-fraction 0.3
      ;; This is needed when `reftex-cite-format' is set. See
      ;; https://superuser.com/a/1386206
      LaTeX-reftex-cite-format-auto-activate nil)

;;-- end reftex

;;-- smartparens
(after! (latex smartparens-latex)
  ;; We have to use lower case modes here, because `smartparens-mode' uses
  ;; the same during configuration.
  (let ((modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)))
    ;; All these excess pairs dramatically slow down typing in LaTeX buffers,
    ;; so we remove them. Let snippets do their job.
    (dolist (open '("\\left(" "\\left[" "\\left\\{" "\\left|"
                    "\\bigl(" "\\biggl(" "\\Bigl(" "\\Biggl(" "\\bigl["
                    "\\biggl[" "\\Bigl[" "\\Biggl[" "\\bigl\\{" "\\biggl\\{"
                    "\\Bigl\\{" "\\Biggl\\{"
                    "\\lfloor" "\\lceil" "\\langle"
                    "\\lVert" "\\lvert" "`"))
      (sp-local-pair modes open nil :actions :rem))
    ;; And tweak these so that users can decide whether they want use LaTeX
    ;; quotes or not, via `+latex-enable-plain-double-quotes'.
    (sp-local-pair modes "``" nil :unless '(:add sp-in-math-p)))

  )

;;-- end smartparens

;;-- specs
(spec-handling-add! company
                    '(reftex-mode (:mode . company-reftex-labels) (:mode . company-reftex-citations))
                    '(LaTeX-mode (:mode . company-auctex-environments) (:mode . company-auctex-macros) (:mode . +latex-symbols-company-backend))
                    )
(spec-handling-add! lookup-url
                    '(latex
                      ("Latex Packages" "https://www.ctan.org/search?phrase=%s")
                      )
                    )
(spec-handling-add! popup
                    '(latex
                      (" output\\*$" :size 15)
                      ("^\\*TeX \\(?:Help\\|errors\\)"  :size 0.3 :select t :ttl nil)
                      ("^\*latex\*" :side right  :ttl nil :width  0.5 :quit nil :select t :priority 50)
                      )
                    )
(spec-handling-add! lookup-regular
                    '(LaTeX-mode
                     ("Latex Wikibook" . "https://en.m.wikibooks.org/wiki/LaTeX")
                     ("CTAN packages" . "https://www.ctan.org/pkg/latex")
                     ("Overleaf docs" . "https://www.overleaf.com/learn")
                     ("Latex Tutorial" . "https://latex-tutorial.com/")
                     ("Fonts" . "https://tug.org/FontCatalogue/")
                     ("Font Installation" . "https://tug.org/fonts/fontinstall.html")
                     )
                    )
(spec-handling-add! evil-embrace
                    '(LaTeX-mode
                      (?\' . ,(make-embrace-pair-struct
                               :key ?\'
                               :left "`"
                               :right "\'"
                               :left-regexp (regexp-quote "`")
                               :right-regexp (regexp-quote "\'")
                               ))
                      (?\' . ,(make-embrace-pair-struct
                               :key ?\"
                               :left "``"
                               :right "\'\'"
                               :left-regexp (regexp-quote "``")
                               :right-regexp (regexp-quote "\'\'")
                               ))
                      (?l . ,(make-embrace-pair-struct
                              :key ?l
                              :left-regexp "\\[a-z+]{"
                              :right-regexp "}"
                              :read-function #'+evil--embrace-latex
                              ))
                      )
                    )
(spec-handling-add! auto-modes
                    '(latex
                      ("\\.tex\\'" . LaTeX-mode)
                      )
                    )

(spec-handling-add! compile-commands
                    '(latex +jg-latex-get-commands)
                    )
;;-- end specs
