;;; lang/latex/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(after! tex
  ;; Set-up chktex.
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  (setq-hook! 'TeX-mode-hook
    ;; Tell Emacs how to parse TeX files.
    ispell-parser 'tex
    ;; Don't auto-fill in math blocks.
    fill-nobreak-predicate (cons #'texmathp fill-nobreak-predicate))
  ;; Enable `rainbow-mode' after applying styles to the buffer.
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)

  ;; Hook LSP, if enabled.
  (when (modulep! +lsp)
    (add-hook! '(tex-mode-local-vars-hook
                 latex-mode-local-vars-hook)
               :append #'lsp!))
  )
(after! latex
  ;; Add the TOC entry to the sectioning hooks.
  (setq LaTeX-section-hook '(LaTeX-section-heading
                             LaTeX-section-title
                             LaTeX-section-toc
                             LaTeX-section-section
                             LaTeX-section-label)
        LaTeX-fill-break-at-separators nil
        LaTeX-item-indent 0)
  ;; (defvar TeX-view-program-selection nil)
  ;; (defvar TeX-view-program-list nil)
  ;; (load! "+viewers")
  )

(use-package! tex-mode
  :defer t
  :config
  (defvar LaTeX-indent-environment-list nil)
  (load! "+fontification")
  ;; Provide proper indentation for LaTeX "itemize", "enumerate", and
  ;; "description" environments. See
  ;; http://emacs.stackexchange.com/questions/3083/how-to-indent-items-in-latex-auctex-itemize-environments.
  ;; Set `+latex-indent-item-continuation-offset' to 0 to disable this.
  (dolist (env '("itemize" "enumerate" "description"))
    (add-to-list 'LaTeX-indent-environment-list `(,env +latex-indent-item-fn)))
  )

(use-package! tex-fold
  :defer t
  :when (modulep! +fold)
  :hook (TeX-mode . +latex-TeX-fold-buffer-h)
  :hook (TeX-mode . TeX-fold-mode)
  :config
  (defun +latex-TeX-fold-buffer-h ()
    (run-with-idle-timer 0 nil 'TeX-fold-buffer))
  ;; Fold after all AUCTeX macro insertions.
  (advice-add #'TeX-insert-macro :after #'+latex-fold-last-macro-a)
  ;; Fold after CDLaTeX macro insertions.
  (advice-add #'cdlatex-math-symbol :after #'+latex-fold-last-macro-a)
  (advice-add #'cdlatex-math-modify :after #'+latex-fold-last-macro-a)
  ;; Fold after snippets.
  (when (modulep! :editor snippets)
    (add-hook! 'TeX-fold-mode-hook
      (defun +latex-fold-snippet-contents-h ()
        (add-hook! 'yas-after-exit-snippet-hook :local
          (when (and yas-snippet-beg yas-snippet-end)
            (TeX-fold-region yas-snippet-beg yas-snippet-end))))))

  (add-hook! 'mixed-pitch-mode-hook
    (defun +latex-fold-set-variable-pitch-h ()
      "Fix folded things invariably getting fixed pitch when using mixed-pitch.
Math faces should stay fixed by the mixed-pitch blacklist, this is mostly for
\\section etc."
      (when mixed-pitch-mode
        ;; Adding to this list makes mixed-pitch clean the face remaps after us
        (add-to-list 'mixed-pitch-fixed-cookie
                     (face-remap-add-relative
                      'TeX-fold-folded-face
                      :family (face-attribute 'variable-pitch :family)
                      :height (face-attribute 'variable-pitch :height))))))
  )

(use-package! preview
  :defer t
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))
  ;; Don't cache preamble, it creates issues with SyncTeX. Let users enable
  ;; caching if they have compilation times that long.
  (setq preview-auto-cache-preamble nil)
  )

(use-package! cdlatex
  :defer t
  :when (modulep! +cdlatex)
  :hook (LaTeX-mode . cdlatex-mode)
  :hook (org-mode . org-cdlatex-mode)
  :config
  ;; Use \( ... \) instead of $ ... $.
  (setq cdlatex-use-dollar-to-ensure-math nil)
  )

(use-package! adaptive-wrap
  :defer t
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0)
  ;; Nicely indent lines that have wrapped when visual line mode is activated.
  )

(use-package! auctex-latexmk
  :defer t
  :when (modulep! +latexmk)
  :after latex
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active.
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Set LatexMk as the default.
  (setq-hook! LaTeX-mode TeX-command-default "LatexMk")
  :config
  ;; Add LatexMk as a TeX target.
  (auctex-latexmk-setup)
  )

(use-package! evil-tex
  :defer t
  :when (modulep! :editor evil +everywhere)
  :hook (LaTeX-mode . evil-tex-mode))

(use-package! company-auctex
  :when (modulep! :completion company)
  :defer t
  )

(use-package! company-math
  :when (modulep! :completion company)
  :defer t
  )

(use-package! reftex
  :hook (LaTeX-mode . reftex-mode)
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

(use-package! auctex :defer t)
