;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-latex-mode-map (make-sparse-keymap))
(setq-default TeX-master t)

(spec-handling-setq! latex 50
                     ;; Use hidden directories for AUCTeX files.
                     TeX-auto-local (expand-file-name "auctex/auto" user-cache-dir)
                     TeX-style-local(expand-file-name "auctex/style" user-cache-dir)
                     TeX-parse-self t ; parse on load
                     TeX-auto-save t  ; parse on save
                     TeX-source-correlate-mode t
                     TeX-source-correlate-method 'synctex
                     TeX-source-correlate-start-server nil ;; Don't start the Emacs server when correlating sources.
                     TeX-electric-sub-and-superscript t    ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
                     TeX-save-query nil                    ;; Just save, don't ask before each compilation.
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

;;-- fold settings
;; TeX-fold-macro-spec-list
;; TeX-fold-math-spec-list
(setq TeX-fold-env-spec-list '(("[comment]" ("comment"))
                               ("[figure]"  ("figure" "figure*"))
                               ("[Proof]"   ("prooftree" "NatD"))
                               ("[Equation]" ("equation"))
                               ))


;;-- end fold settings

;;-- specs
(spec-handling-add! company
                    '(reftex-mode (:mode company-reftex-labels company-reftex-citations))
                    '(LaTeX-mode (:mode company-auctex-environments company-auctex-macros +latex-symbols-company-backend))
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
                      ("\\*latex-check\\*\\'"  :ttl nil :height 0.2 :quit t :select nil :priority 60)
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
                      ("\\.sty\\'" . LaTeX-mode)
                      ("\\.bbl\\'" . latex-mode)
                      )
                    )
(spec-handling-add! file-templates
                    '(latex
                      ("\\.sty\\'" :trigger "__sty" :mode latex-mode :priority -95)
                      ("\\.tex\\'" :trigger "__"               :mode latex-mode :priority -99)
                      (LaTeX-mode :trigger "__" :priority -100)
                      (latex-mode :trigger "__" :priority -100)
                      )
                    )
(spec-handling-add! compile-commands
                    '(latex +jg-latex-get-commands)
                    )
(spec-handling-add! babel
                    '(latex
                      (:name latex      :lib ob-latex)
                      (:name lilypond   :lib ob-lilypond)
                      )
                    )
(spec-handling-add! fold
                    `(latex
                      :modes (latex-mode LaTeX-mode TeX-fold-mode)
                      :priority 25
                      :triggers (:open-all   ,#'TeX-fold-clearout-buffer
                                 :close-all  ,#'TeX-fold-buffer
                                 :toggle     ,#'TeX-fold-dwim
                                 :open       nil
                                 :open-rec   nil
                                 :close      nil
                                 )
                      )
                    )
;;-- end specs
