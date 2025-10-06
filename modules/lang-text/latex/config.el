;;; lang/latex/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+defs")
(local-load! "+extra-config")

(defer-load! jg-bindings-total "+bindings")

(advice-add 'Latex-fill-region-as-paragraph :around #'+latex-dont-indent-itemize-and-enumerate-a)
(advice-add 'Latex-fill-region-as-para-do   :around #'+latex-re-indent-itemize-and-enumerate-a)
;; Fold after all AUCTeX macro insertions.
(advice-add 'TeX-insert-macro :after #'+latex-fold-last-macro-a)
;; Fold after CDLaTeX macro insertions.
(advice-add 'cdlatex-math-symbol :after #'+latex-fold-last-macro-a)
(advice-add 'cdlatex-math-modify :after #'+latex-fold-last-macro-a)


(use-package! tex-mode) ;; built-in

(use-package! auctex
  :disabled t
  )

(use-package! tex ;; part of auctex
  :after auctex
  :config
  (defvar LaTeX-indent-environment-list nil)
  (load! "+fontification")
  ;; Provide proper indentation for LaTeX "itemize", "enumerate", and
  ;; "description" environments. See
  ;; http://emacs.stackexchange.com/questions/3083/how-to-indent-items-in-latex-auctex-itemize-environments.
  ;; Set `+latex-indent-item-continuation-offset' to 0 to disable this.
  (dolist (env '("itemize" "enumerate" "description"))
    (add-to-list 'LaTeX-indent-environment-list `(,env +latex-indent-item-fn)))

  ;; Set-up chktex.
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  (setq-hook! 'TeX-mode-hook
    ;; Tell Emacs how to parse TeX files.
    ispell-parser 'tex
    ;; Don't auto-fill in math blocks.
    fill-nobreak-predicate (cons #'texmathp fill-nobreak-predicate))
  ;; Enable `rainbow-mode' after applying styles to the buffer.
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)
  )

(use-package! latex ;; part of auctex
  :after auctex
  :config
  (puthash "dropcap" #'+jg-latex-dropcap-opt-ivy jg-latex-insert-ivys)

  ;; Add the TOC entry to the sectioning hooks.
  (setq LaTeX-section-hook '(LaTeX-section-heading
                             LaTeX-section-title
                             LaTeX-section-toc
                             LaTeX-section-section
                             LaTeX-section-label)
        LaTeX-fill-break-at-separators nil
        LaTeX-item-indent 0
        )

  (add-hook! 'LaTeX-mode-hook
             #'librarian-insert-minor-mode
             )

  (setq-hook! 'LaTeX-mode-hook
    jg-ivy-general-insert-sub-ivys jg-latex-insert-ivys
    )
  ;; (defvar TeX-view-program-selection nil)
  ;; (defvar TeX-view-program-list nil)
  ;; (load! "+viewers")
  )

(use-package! tex-fold ;; part of auctex
  :after auctex
  :config
  (defun +latex-TeX-fold-buffer-h ()
    (run-with-idle-timer 0 nil 'TeX-fold-buffer))
  ;; Fold after snippets.
  (add-hook! 'TeX-fold-mode-hook #'+latex-fold-snippet-contents-h)
  (add-hook! 'mixed-pitch-mode-hook #'+latex-fold-set-variable-pitch-h)
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode)
  (add-hook 'LaTex-mode-hook #'+latex-TeX-fold-buffer-h)
  )

(use-package! preview ;; part of auctex
  :after auctex
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function #'+latex-preview-scale-fn
                )
  ;; Don't cache preamble, it creates issues with SyncTeX. Let users enable
  ;; caching if they have compilation times that long.
  (setq preview-auto-cache-preamble nil)
  )

(use-package! reftex ;; built-in
  :commands reftex-mode
  :config
  ;; Set up completion for citations and references.
  (add-hook 'reftex-mode-hook #'evil-normalize-keymaps)

  (add-hook! 'reftex-toc-mode-hook
    (reftex-toc-rescan)
    (map! :map 'local
          :e "j"   #'next-line
          :e "k"   #'previous-line
          :e "q"   #'kill-buffer-and-window
          :e "ESC" #'kill-buffer-and-window
          )
    )
  )
