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

(defvar +latex--company-backends nil)

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
      TeX-save-query nil)

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

;;-- browse providers
(after! jg-browse-providers
  (pushnew! jg-browse-providers-alist
            '("Latex Packages" "https://www.ctan.org/search?phrase=%s")
            )
  )

;;-- end browse providers

;;-- popup
(setq jg-latex-popup-rules
      '(
        (" output\\*$" :size 15)
        ("^\\*TeX \\(?:Help\\|errors\\)"  :size 0.3 :select t :ttl nil)
        ("^\*latex\*"         :side right  :ttl nil :width  0.5 :quit nil :select t :priority 50)
        ))
(after! jg-popup-init
  (+jg-ui-popup-add-rules 'latex jg-latex-popup-rules)
  )
;;-- end popup
