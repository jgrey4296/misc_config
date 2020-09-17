;; jg_emacs config.el
;; loaded fourth

(message "Configuring Personal Preferences")
(setq-default
              flycheck-display-errors-function nil
              flycheck-help-echo-function nil
              flycheck-process-error-functions nil
              helm-truncate-lines t
              icicle-Completions-text-scale-decrease 0
              indent-tabs-mode nil
              jg-bookmark-location "~/github/writing/other_files/main_bookmarks.html"
              jg-spacemacs-main-layer/dired-recursive-switches "-aBhlR --group-directories-first"
              jg-spacemacs-main-layer/major-modes '(ada-mode
                                                    ams-tex-mode antlr-mode
                                                    array-mode asm-mode autoconf-mode awk-mode
                                                    bat-mode biblio-selection-mode bibtex-mode
                                                    bibtex-style-mode bookmark-bmenu-mode bookmark-edit-annotation-mode
                                                    bovine-grammar-mode Buffer-menu-mode c++-mode c-mode calendar-mode
                                                    change-log-mode cmake-mode cmm-mode coffee-mode comint-mode
                                                    common-lisp-mode completion-list-mode completion-mode context-en-mode
                                                    context-mode context-nl-mode cperl-mode csharp-mode css-mode csv-mode
                                                    Custom-mode cvs-status-mode cython-mode d-mode data-debug-mode dcl-mode
                                                    debugger-mode decipher-mode diary-fancy-display-mode diary-mode diff-mode
                                                    dns-mode doc-view-mode doctex-mode docTeX-mode dotspacemacs-mode dsssl-mode ebrowse-member-mode
                                                    ebrowse-tree-mode edebug-Continue-fast-mode edebug-continue-mode edebug-eval-mode
                                                    edebug-Go-nonstop-mode edebug-next-mode edebug-set-initial-mode edebug-step-mode
                                                    edebug-step-through-mode edebug-Trace-fast-mode edebug-trace-mode
                                                    edit-abbrevs-mode edmacro-mode eieio-custom-mode electric-describe-mode
                                                    elisp-byte-code-mode emacs-lisp-mode epa-info-mode epa-key-list-mode epa-key-mode erlang-mode
                                                    eshell-mode evil-command-window-mode evil-list-view-mode eww-mode
                                                    trie-explore-mode f90-mode flycheck-error-list-mode flycheck-error-message-mode
                                                    flymake-diagnostics-buffer-mode flyspell-prog-mode fortran-mode
                                                    free-keys-mode fsharp-mode fundamental-mode gdb-script-mode gfm-mode gfm-view-mode
                                                    ggtags-global-mode ggtags-view-search-history-mode ggtags-view-tag-history-mode ghc-core-mode
                                                    ghci-script-mode glsl-mode gnuplot-mode gnus-group-enter-server-mode
                                                    gnus-group-mode gnus-score-mode go-dot-mod-mode go-mode graphviz-dot-mode grep-mode
                                                    haml-mode haskell-c2hs-mode haskell-cabal-mode haskell-mode help-mode
                                                    html-erb-mode html-mode hy-mode ibuffer-mode icon-mode
                                                    idl-mode idlwave-mode image-mode indent-according-to-mode indented-text-mode
                                                    inf-ruby-mode inferior-hy-mode Info-edit-mode isearch-describe-mode java-mode
                                                    javascript-mode js-jsx-mode js-mode js2-jsx-mode js2-mode json-mode latex-mode
                                                    latex/auto-fill-mode ld-script-mode less-css-mode
                                                    lisp-interaction-mode lisp-mode literate-haskell-mode log-edit-mode log-view-mode
                                                    log4e-mode lua-mode m2-mode m4-mode magit-mode
                                                    mail-mode makefile-automake-mode makefile-bsdmake-mode makefile-gmake-mode
                                                    makefile-imake-mode makefile-makepp-mode makefile-mode Man-mode markdown-mode markdown-view-mode
                                                    mercury-mode message-mode messages-buffer-mode metafont-mode metapost-mode mh-folder-mode
                                                    mhtml-mode minibuffer-inactive-mode mixal-mode nroff-mode nxml-mode nxml-web-mode
                                                    objc-mode occur-edit-mode occur-mode octave-mode opascal-mode org-mode outline-mode
                                                    package-menu-mode paragraph-indent-text-mode pascal-mode pdf-virtual-edit-mode pdf-virtual-view-mode
                                                    perl-mode picture-mode pike-mode pip-requirements-mode plain-tex-mode plantuml-mode
                                                    plstore-mode pr-txt-mode process-menu-mode profiler-report-mode prog-mode
                                                    prolog-inferior-mode prolog-mode ps-mode pug-mode python-mode quickurl-list-mode
                                                    racket-mode reb-enter-subexp-mode reb-lisp-mode reb-mode reb-quit-subexp-mode
                                                    recentf-dialog-mode reftex-index-phrases-mode reftex-reset-mode reftex-select-bib-mode
                                                    reftex-select-label-mode rmail-mode rst-mode ruby-mode rust-mode rxt-help-mode sass-mode
                                                    scheme-mode scss-mode select-tags-table-mode trie-sequence-mode ses-mode sgml-mode
                                                    sgml-name-8bit-mode sh-mode shell-mode sieve-mode simula-mode slim-mode slitex-mode
                                                    snippet-mode snmp-mode snmpv2-mode spacemacs-buffer-mode special-mode speedbar-mode
                                                    sql-mode srecode-template-mode srefactor-ui-menu-mode tablist-mode tabulated-list-mode
                                                    tags-table-mode tar-mode tcl-mode term-char-mode term-line-mode term-mode TeX-latex-mode
                                                    tex-mode TeX-plain-tex-mode TeX-tex-mode TeX-texinfo-mode texinfo-mode text-mode
                                                    todo-archive-mode todo-filtered-items-mode todo-mode toml-mode trie-mode trie-passive-mode
                                                    trie-log-mode tsv-mode url-cookie-mode
                                                    use-package-statistics-mode vera-mode verilog-mode vhdl-mode viper-mode wdired-change-to-wdired-mode web-mode
                                                    which-key-show-major-mode wisent-grammar-mode woman-mode xref--xref-buffer-mode yaml-mode)

              )

(load! "+bindings")
(load! "+funcs")
(load! "+python-origami")
(load! "+evil-ex-setup")

(after! (evil evil-snipe)
  (push 'dired-mode evil-snipe-disabled-modes)

  )
(use-package! hl-line
  :init
  (global-hl-line-mode)
  )
(after! yasnippet
  ;; If an error occurs, change yas-installed-snippets-dir is not in yas-snippet-dirs
  ;; as it is obsolete
  (setq yas-snippet-dirs `(,(expand-file-name "~/.doom.d/snippets/")))
  ;; ,(expand-file-name "~/github/otherLibs/yasnippet-snippets/snippets")
  ;; ,(expand-file-name "~/github/otherLibs/yasnippet-snippets")
  )
(after! evil-quickscope
  ;; TODO (spacemacs/set-leader-keys "t q" 'jg-spacemacs-main-layer/toggle-quickscope-always)
  (global-evil-quickscope-always-mode 1)
  )
(use-package! hi-lock
  :init
  (global-hi-lock-mode)
  :config
  (setq hi-lock-auto-select-face t)
  )
(after! (evil hl-line)
  (message "Adjusting up hl-line")
  ;; Set up faces for hl-line colour sync to status
  (defface evil-normal-state '((t :background  "#000000")) "The Evil Normal State Hl-line")
  (defface evil-insert-state '((t :background  "#005f00")) "The Evil Insert State Hl-line")
  (defface evil-visual-state '((t :background  "#005fff")) "The Evil Visual State Hl-line")
  (defface evil-motion-state '((t :background  "#5f0000")) "The Evil Motion State Hl-line")
  (defface evil-emacs-state '((t :background  "#5f00ff"))  "The Evil Emacs State Hl-line")
  (defface evil-replace-state '((t :background  "#8700ff")) "The Evil Replace State Hl-line")
  (defface evil-hybrid-state '((t :background  "#0087ff")) "The Evil Hybrid State Hl-line")
  (defface evil-evilified-state '((t :background  "#5f5f00")) "The Evil Evilified State Hl-line")
  (defface evil-lisp-state '((t :background  "#875fff")) "The Evil Lisp State Hl-line")
  (defface evil-iedit-state '((t :background  "#8700af")) "The Evil iedit State Hl-line")
  (defface evil-iedit-insert-state '((t :background  "#8700af")) "The Iedit Insert state Hl-line")

  ;; hooks for evil state entry hooks to change hl-line colour
 (add-hook 'evil-normal-state-entry-hook       (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-normal-state))))
 (add-hook 'evil-insert-state-entry-hook       (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-insert-state))))
 (add-hook 'evil-visual-state-entry-hook       (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-visual-state))))
 (add-hook 'evil-motion-state-entry-hook       (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-motion-state))))
 (add-hook 'evil-emacs-state-entry-hook        (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-emacs-state))))
 (add-hook 'evil-replace-state-entry-hook      (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-replace-state))))
 (add-hook 'evil-hybrid-state-entry-hook       (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-hybrid-state))))
 (add-hook 'evil-evilified-state-entry-hook    (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-evilified-state))))
 (add-hook 'evil-lisp-state-entry-hook         (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-lisp-state))))
 (add-hook 'evil-iedit-state-entry-hook        (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-iedit-state))))
 (add-hook 'evil-iedit-insert-state-entry-hook (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-iedit-insert-state))))
  )
(after! (dired dired-quick-sort)
  (setq dired-quick-sort-group-directories-last ?y)
  )
(after! ibuffer
  (add-transient-hook! 'ibuffer-hook 'jg-spacemacs-main-layer/setup-ibuffer)
  )

;; separator
(when (featurep! +inactive)

  (after! helm
    (helm-autoresize-mode 0)
    ;;add in keybinding to kill line in completion window
    (define-key helm-map (kbd "C-k") 'kill-line)
    (define-key helm-map (kbd "<backtab>") 'helm-select-action)
    (setq helm-find-files-actions
          (append `(,(car helm-find-files-actions))
                  '(("Open Random" . jg-spacemacs-main-layer/helm-open-random-action))
                  '(("Describe Random" . jg-spacemacs-main-layer/helm-describe-random-action))
                  '(("Open Random External" . jg-spacemacs-main-layer/helm-open-random-external-action))
                  (cdr helm-find-files-actions))
          )
    ;; Overwrite completion layer RET binding (line 185 of +completion/helm/packages file)
    (define-key helm-find-files-map
      (kbd "RET") nil)
    )
  (after! python
    (setq-default python-indent-offset 4
                  python-indent-guess-indent-offset nil
                  python-shell-interpreter-args "-i"
                  python-shell-interpreter "python"
                  python-shell-completion-native-enable t
                  python-shell-virtualenv-root "~/anaconda3/envs"
                  python-shell--interpreter nil
                  python-shell--interpreter-args nil
                  )

    (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
    ;; (add-hook 'python-mode-hook #'(lambda ()
    (map! :mode python-mode
          :localleader
          "db" 'jg-spacemacs-main-layer/python-toggle-breakpoint
          )
    (evil-define-key 'normal python-mode-map
      (kbd "z d") 'jg-spacemacs-main-layer/toggle-all-defs
      (kbd "z C") 'jg-spacemacs-main-layer/close-class-defs
      )
    )
  (after! flycheck
    (defun flycheck-finish-checker-process
        (checker exit-status files output callback cwd)
      """ Custom flycheck finisher to stop annoying 'suspicious' errors """
      (let ((errors (flycheck-parse-output output checker (current-buffer))))
        (funcall callback 'finished
                 ;; Fix error file names, by substituting them backwards from the
                 ;; temporaries.
                 (seq-map (lambda (e) (flycheck-fix-error-filename e files cwd))
                          errors))))
    )
  (after! origami
    (require 'jg-spacemacs-main-layer/origami-python-parser "~/.doom.d/modules/jg-spacemacs-main-layer/local/origami-parser.el")
    (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
    (add-to-list 'origami-parser-alist '(python-mode . jg-spacemacs-main-layer/origami-python-parser))
    )
  (after! vlf
    (define-key evil-normal-state-map (kbd "] A") 'vlf-next-batch-from-point)
    (define-key evil-normal-state-map (kbd "] a") 'vlf-next-batch)
    (define-key evil-normal-state-map (kbd "[ a") 'vlf-prev-batch)
    ;; TODO
    ;; (spacemacs/set-leader-keys "a U v " 'vlf-set-batch-size))
    )

  (after! neotree
    (push "^__pycache__$" neo-hidden-regexp-list)
    (push "^G\\(PATH\\|R?TAGS\\)$" neo-hidden-regexp-list)
    (push "^__init__.py$" neo-hidden-regexp-list)
    )
  (after! pyenv
    """ Remove the annoying python-shell-setup advice """
    (add-transient-hook! 'dired-mode
      (define-key dired-mode-map
        "v" 'pyvenv-activate
        )
      )
    )
  (after! helm-gtags
    ;; Adapated from helm-gtags spacemacs layer
    ;; (let ((jumpl (intern (format "spacemacs-jump-handlers-%S" 'python-mode))))
    ;;   (when (boundp jumpl)
    ;;     (add-to-list jumpl 'spacemacs/helm-gtags-maybe-dwim 'append)))

    (map! :mode 'python-mode
          "gC" 'helm-gtags-create-tags
          "gd" 'helm-gtags-find-tag
          "gD" 'helm-gtags-find-tag-other-window
          "gG" 'helm-gtags-dwim-other-window
          "gi" 'helm-gtags-tags-in-this-function
          "gl" 'helm-gtags-parse-file
          "gn" 'helm-gtags-next-history
          "gp" 'helm-gtags-previous-history
          "gr" 'helm-gtags-find-rtag
          "gR" 'helm-gtags-resume
          "gs" 'helm-gtags-select
          "gS" 'helm-gtags-show-stack
          "gy" 'helm-gtags-find-symbol
          "gU" 'helm-gtags-update-tags
          )
    )
  )
