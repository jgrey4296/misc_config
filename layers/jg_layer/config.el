;; jg_emacs config.el
;; loaded fourth

(setq-default auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/`" t)))
              backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
              display-buffer-alist (quote (("*shell*" display-buffer-same-window (nil))))
              icicle-Completions-text-scale-decrease 0
              ;;personal iSpell dictionary
              ispell-personal-dictionary (expand-file-name "~/.spacemacs.d/setup_files/.ispell_english")
              ;;Dired customisation
              dired-omit-files "^\\.?#\\|^\\.$\\|^\\.DS_Store$\\|^\\.git$\\|^__pycache__$\\|^flycheck__.+\\.py"
              dired-omit-verbose nil
              dired-listing-switches "-lha"
              ;;use spaces instead of tabs
              tab-width 4
              indent-tabs-mode nil
              ;;bookmark location
              jg-bookmark-location "~/github/writing/other_files/main_bookmarks.html"
              ;;Flycheck
              flycheck-display-errors-function nil
              flycheck-help-echo-function nil
              flycheck-process-error-functions nil
              ;;evil cursor control:
              evil-move-beyond-eol t
              ;; use helm-mode-manager-list-major-modes to rebuild and prune:
              jg_layer/major-modes '(ada-mode ams-tex-mode antlr-mode array-mode asm-mode autoconf-mode awk-mode bat-mode biblio-selection-mode bibtex-mode bibtex-style-mode bookmark-bmenu-mode bookmark-edit-annotation-mode bovine-grammar-mode Buffer-menu-mode c++-mode c-mode calendar-mode change-log-mode cmake-mode cmm-mode coffee-mode comint-mode common-lisp-mode completion-list-mode completion-mode context-en-mode context-mode context-nl-mode cperl-mode csharp-mode css-mode csv-mode Custom-mode cvs-status-mode cython-mode d-mode data-debug-mode dcl-mode debugger-mode decipher-mode diary-fancy-display-mode diary-mode diff-mode dns-mode doc-view-mode doctex-mode docTeX-mode dotspacemacs-mode dsssl-mode ebrowse-member-mode ebrowse-tree-mode edebug-Continue-fast-mode edebug-continue-mode edebug-eval-mode edebug-Go-nonstop-mode edebug-next-mode edebug-set-initial-mode edebug-step-mode edebug-step-through-mode edebug-Trace-fast-mode edebug-trace-mode edit-abbrevs-mode edmacro-mode eieio-custom-mode electric-describe-mode elisp-byte-code-mode emacs-lisp-mode epa-info-mode epa-key-list-mode epa-key-mode erlang-mode eshell-mode evil-command-window-mode evil-list-view-mode eww-mode trie-explore-mode f90-mode flycheck-error-list-mode flycheck-error-message-mode flymake-diagnostics-buffer-mode flyspell-prog-mode fortran-mode free-keys-mode fsharp-mode fundamental-mode gdb-script-mode gfm-mode gfm-view-mode ggtags-global-mode ggtags-view-search-history-mode ggtags-view-tag-history-mode ghc-core-mode ghci-script-mode glsl-mode gnuplot-mode gnus-group-enter-server-mode gnus-group-mode gnus-score-mode go-dot-mod-mode go-mode graphviz-dot-mode grep-mode haml-mode haskell-c2hs-mode haskell-cabal-mode haskell-mode help-mode html-erb-mode html-mode hy-mode ibuffer-mode icon-mode idl-mode idlwave-mode image-mode indent-according-to-mode indented-text-mode inf-ruby-mode inferior-hy-mode Info-edit-mode isearch-describe-mode java-mode javascript-mode js-jsx-mode js-mode js2-jsx-mode js2-mode json-mode latex-mode latex/auto-fill-mode ld-script-mode less-css-mode lisp-interaction-mode lisp-mode literate-haskell-mode log-edit-mode log-view-mode log4e-mode lua-mode m2-mode m4-mode magit-mode mail-mode makefile-automake-mode makefile-bsdmake-mode makefile-gmake-mode makefile-imake-mode makefile-makepp-mode makefile-mode Man-mode markdown-mode markdown-view-mode mercury-mode message-mode messages-buffer-mode metafont-mode metapost-mode mh-folder-mode mhtml-mode minibuffer-inactive-mode mixal-mode nroff-mode nxml-mode nxml-web-mode objc-mode occur-edit-mode occur-mode octave-mode opascal-mode org-mode outline-mode package-menu-mode paragraph-indent-text-mode pascal-mode pdf-virtual-edit-mode pdf-virtual-view-mode perl-mode picture-mode pike-mode pip-requirements-mode plain-tex-mode plantuml-mode plstore-mode pr-txt-mode process-menu-mode profiler-report-mode prog-mode prolog-inferior-mode prolog-mode ps-mode pug-mode python-mode quickurl-list-mode racket-mode reb-enter-subexp-mode reb-lisp-mode reb-mode reb-quit-subexp-mode recentf-dialog-mode reftex-index-phrases-mode reftex-reset-mode reftex-select-bib-mode reftex-select-label-mode rmail-mode rst-mode ruby-mode rust-mode rxt-help-mode sass-mode scheme-mode scss-mode select-tags-table-mode trie-sequence-mode ses-mode sgml-mode sgml-name-8bit-mode sh-mode shell-mode sieve-mode simula-mode slim-mode slitex-mode snippet-mode snmp-mode snmpv2-mode spacemacs-buffer-mode special-mode speedbar-mode sql-mode srecode-template-mode srefactor-ui-menu-mode tablist-mode tabulated-list-mode tags-table-mode tar-mode tcl-mode term-char-mode term-line-mode term-mode TeX-latex-mode tex-mode TeX-plain-tex-mode TeX-tex-mode TeX-texinfo-mode texinfo-mode text-mode todo-archive-mode todo-filtered-items-mode todo-mode toml-mode trie-mode trie-passive-mode trie-log-mode tsv-mode url-cookie-mode use-package-statistics-mode vera-mode verilog-mode vhdl-mode viper-mode wdired-change-to-wdired-mode web-mode which-key-show-major-mode wisent-grammar-mode woman-mode xref--xref-buffer-mode yaml-mode)
              ;;Dired recursive:
              jg_layer/dired-recursive-switches "-aBhlR --group-directories-first"

              )
;; force utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
