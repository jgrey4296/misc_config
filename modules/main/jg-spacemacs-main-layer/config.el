;; jg_emacs config.el
;; loaded fourth

(setq-default auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/`" t)))
              backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
              display-buffer-alist (quote (("*shell*" display-buffer-same-window (nil))))
              icicle-Completions-text-scale-decrease 0
              ;;personal iSpell dictionary
              ispell-personal-dictionary (expand-file-name "~/.spacemacs.d/setup_files/.ispell_english")
              ;;Dired customisation
              dired-omit-files "^\\.?#\\|^\\.$\\|^\\.DS_Store$\\|^\\.git$\\|^__pycache__$\\|^flycheck__.+\\.py\\|^\\.mypy_cache$"
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
              jg-spacemacs-main-layer/major-modes '(ada-mode ams-tex-mode antlr-mode array-mode asm-mode autoconf-mode awk-mode bat-mode biblio-selection-mode bibtex-mode bibtex-style-mode bookmark-bmenu-mode bookmark-edit-annotation-mode bovine-grammar-mode Buffer-menu-mode c++-mode c-mode calendar-mode change-log-mode cmake-mode cmm-mode coffee-mode comint-mode common-lisp-mode completion-list-mode completion-mode context-en-mode context-mode context-nl-mode cperl-mode csharp-mode css-mode csv-mode Custom-mode cvs-status-mode cython-mode d-mode data-debug-mode dcl-mode debugger-mode decipher-mode diary-fancy-display-mode diary-mode diff-mode dns-mode doc-view-mode doctex-mode docTeX-mode dotspacemacs-mode dsssl-mode ebrowse-member-mode ebrowse-tree-mode edebug-Continue-fast-mode edebug-continue-mode edebug-eval-mode edebug-Go-nonstop-mode edebug-next-mode edebug-set-initial-mode edebug-step-mode edebug-step-through-mode edebug-Trace-fast-mode edebug-trace-mode edit-abbrevs-mode edmacro-mode eieio-custom-mode electric-describe-mode elisp-byte-code-mode emacs-lisp-mode epa-info-mode epa-key-list-mode epa-key-mode erlang-mode eshell-mode evil-command-window-mode evil-list-view-mode eww-mode trie-explore-mode f90-mode flycheck-error-list-mode flycheck-error-message-mode flymake-diagnostics-buffer-mode flyspell-prog-mode fortran-mode free-keys-mode fsharp-mode fundamental-mode gdb-script-mode gfm-mode gfm-view-mode ggtags-global-mode ggtags-view-search-history-mode ggtags-view-tag-history-mode ghc-core-mode ghci-script-mode glsl-mode gnuplot-mode gnus-group-enter-server-mode gnus-group-mode gnus-score-mode go-dot-mod-mode go-mode graphviz-dot-mode grep-mode haml-mode haskell-c2hs-mode haskell-cabal-mode haskell-mode help-mode html-erb-mode html-mode hy-mode ibuffer-mode icon-mode idl-mode idlwave-mode image-mode indent-according-to-mode indented-text-mode inf-ruby-mode inferior-hy-mode Info-edit-mode isearch-describe-mode java-mode javascript-mode js-jsx-mode js-mode js2-jsx-mode js2-mode json-mode latex-mode latex/auto-fill-mode ld-script-mode less-css-mode lisp-interaction-mode lisp-mode literate-haskell-mode log-edit-mode log-view-mode log4e-mode lua-mode m2-mode m4-mode magit-mode mail-mode makefile-automake-mode makefile-bsdmake-mode makefile-gmake-mode makefile-imake-mode makefile-makepp-mode makefile-mode Man-mode markdown-mode markdown-view-mode mercury-mode message-mode messages-buffer-mode metafont-mode metapost-mode mh-folder-mode mhtml-mode minibuffer-inactive-mode mixal-mode nroff-mode nxml-mode nxml-web-mode objc-mode occur-edit-mode occur-mode octave-mode opascal-mode org-mode outline-mode package-menu-mode paragraph-indent-text-mode pascal-mode pdf-virtual-edit-mode pdf-virtual-view-mode perl-mode picture-mode pike-mode pip-requirements-mode plain-tex-mode plantuml-mode plstore-mode pr-txt-mode process-menu-mode profiler-report-mode prog-mode prolog-inferior-mode prolog-mode ps-mode pug-mode python-mode quickurl-list-mode racket-mode reb-enter-subexp-mode reb-lisp-mode reb-mode reb-quit-subexp-mode recentf-dialog-mode reftex-index-phrases-mode reftex-reset-mode reftex-select-bib-mode reftex-select-label-mode rmail-mode rst-mode ruby-mode rust-mode rxt-help-mode sass-mode scheme-mode scss-mode select-tags-table-mode trie-sequence-mode ses-mode sgml-mode sgml-name-8bit-mode sh-mode shell-mode sieve-mode simula-mode slim-mode slitex-mode snippet-mode snmp-mode snmpv2-mode spacemacs-buffer-mode special-mode speedbar-mode sql-mode srecode-template-mode srefactor-ui-menu-mode tablist-mode tabulated-list-mode tags-table-mode tar-mode tcl-mode term-char-mode term-line-mode term-mode TeX-latex-mode tex-mode TeX-plain-tex-mode TeX-tex-mode TeX-texinfo-mode texinfo-mode text-mode todo-archive-mode todo-filtered-items-mode todo-mode toml-mode trie-mode trie-passive-mode trie-log-mode tsv-mode url-cookie-mode use-package-statistics-mode vera-mode verilog-mode vhdl-mode viper-mode wdired-change-to-wdired-mode web-mode which-key-show-major-mode wisent-grammar-mode woman-mode xref--xref-buffer-mode yaml-mode)
              ;;Dired recursive:
              jg-spacemacs-main-layer/dired-recursive-switches "-aBhlR --group-directories-first"
              ;; Pyvenv default:
              pyvenv-default-virtual-env-name "~/anaconda3/envs/"
              display-line-numbers-width 4
              display-line-numbers t
              ;; Helm
              helm-truncate-lines t
              ;; helm-grep-truncate-lines nil
              ;; helm-occur-truncate-lines nil
              ;; helm-buffers-truncate-lines nil
              ;; helm-projectile-truncate-lines nil
              line-move-ignore-invisible t

              which-key-idle-secondary-delay 0.05
              which-key-sort-order 'which-key-key-order-alpha
              ;; which-key-sort-order 'which-key-prefix-then-key-order
              )
;; force utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)


(defun jg-spacemacs-main-layer/init-s ()
  (use-package s)
  )

(defun jg-spacemacs-main-layer/init-f ()
  (use-package f)
  )

(defun jg-spacemacs-main-layer/pre-init-exec-path-from-shell ()
  ;; (setq exec-path-from-shell-arguments '("-i"))
  )


(defun jg-spacemacs-main-layer/post-init-evil ()
  (setq-default evil-escape-delay 0.3 )
  (global-set-key (kbd "<backtab>") 'evil-normal-state)

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
  (add-hook 'evil-normal-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-normal-state))))
  (add-hook 'evil-insert-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-insert-state))))
  (add-hook 'evil-visual-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-visual-state))))
  (add-hook 'evil-motion-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-motion-state))))
  (add-hook 'evil-emacs-state-entry-hook    (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-emacs-state))))
  (add-hook 'evil-replace-state-entry-hook  (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-replace-state))))
  (add-hook 'evil-hybrid-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-hybrid-state))))
  (add-hook 'evil-evilified-state-entry-hook (lambda () (interactive) (if (overlayp global-hl-line-overlay)    (overlay-put global-hl-line-overlay 'face 'evil-evilified-state))))
  (add-hook 'evil-lisp-state-entry-hook      (lambda () (interactive) (if (overlayp global-hl-line-overlay)    (overlay-put global-hl-line-overlay 'face 'evil-lisp-state))))
  (add-hook 'evil-iedit-state-entry-hook     (lambda () (interactive) (if (overlayp global-hl-line-overlay)    (overlay-put global-hl-line-overlay 'face 'evil-iedit-state))))
  (add-hook 'evil-iedit-insert-state-entry-hook (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-iedit-insert-state))))


  (defvar evil-ex-commands nil
    " Custom Evil-ex commands")

  ;; definition of said commands, adapted from evil-maps

  (evil-ex-define-cmd "cl" 'spacemacs/comment-or-uncomment-lines)
  (evil-ex-define-cmd "mw" 'spacemacs/window-manipulation-transient-state/body)
  (evil-ex-define-cmd "mb" 'spacemacs/buffer-transient-state/body)
  (evil-ex-define-cmd "os" 'org-store-link)
  (evil-ex-define-cmd "oi" 'org-insert-last-stored-link)
  (evil-ex-define-cmd "oo" 'jg-spacemacs-main-layer/open-link)
  (evil-ex-define-cmd "OO" 'jg-spacemacs-main-layer/open-link-externally)
  ;; TODO: registers
  ;; TODO: yasnippet

  ;; file / buffer loading
  (evil-ex-define-cmd "e[dit]" 'evil-edit)
  (evil-ex-define-cmd "w[rite]" 'evil-write)
  (evil-ex-define-cmd "wa[ll]" 'evil-write-all)
  (evil-ex-define-cmd "sav[eas]" 'evil-save)
  (evil-ex-define-cmd "r[ead]" 'evil-read)
  (evil-ex-define-cmd "b[uffer]" 'evil-buffer)

  (evil-ex-define-cmd "bn[ext]" 'evil-next-buffer)
  (evil-ex-define-cmd "bp[revious]" 'evil-prev-buffer)
  (evil-ex-define-cmd "bN[ext]" "bprevious")
  (evil-ex-define-cmd "sb[uffer]" 'evil-split-buffer)
  (evil-ex-define-cmd "sbn[ext]" 'evil-split-next-buffer)
  (evil-ex-define-cmd "sbp[revious]" 'evil-split-prev-buffer)
  (evil-ex-define-cmd "sbN[ext]" "sbprevious")

  ;; buffer and file listings
  (evil-ex-define-cmd "buffers" 'buffer-menu)
  (evil-ex-define-cmd "files" 'evil-show-files)
  (evil-ex-define-cmd "ls" "buffers")

  ;; deleting / yanking
  ;; (evil-ex-define-cmd "c[hange]" 'evil-change)
  (evil-ex-define-cmd "c[opy]" 'evil-copy)
  (evil-ex-define-cmd "m[ove]" 'evil-move)
  (evil-ex-define-cmd "d[elete]" 'evil-ex-delete)
  (evil-ex-define-cmd "y[ank]" 'evil-ex-yank)
  ;;goto top of buffer
  (evil-ex-define-cmd "top" 'evil-goto-char)
  ;; bring a line up
  (evil-ex-define-cmd "j[oin]" 'evil-ex-join)
  ;; alignment
  (evil-ex-define-cmd "le[ft]" 'evil-align-left)
  (evil-ex-define-cmd "ri[ght]" 'evil-align-right)
  (evil-ex-define-cmd "ce[nter]" 'evil-align-center)
  ;; windows/buffer creation
  (evil-ex-define-cmd "sp[lit]" 'evil-window-split)
  (evil-ex-define-cmd "vs[plit]" 'evil-window-vsplit)
  (evil-ex-define-cmd "new" 'evil-window-new)
  (evil-ex-define-cmd "ene[w]" 'evil-buffer-new)
  (evil-ex-define-cmd "vne[w]" 'evil-window-vnew)
  ;; window deletion
  (evil-ex-define-cmd "clo[se]" 'evil-window-delete)
  (evil-ex-define-cmd "on[ly]" 'delete-other-windows)
  ;; quitting emacs
  (evil-ex-define-cmd "q[uit]" 'evil-quit)
  (evil-ex-define-cmd "wq" 'evil-save-and-close)
  (evil-ex-define-cmd "quita[ll]" 'evil-quit-all)
  (evil-ex-define-cmd "qa[ll]" "quitall")
  (evil-ex-define-cmd "cq[uit]" 'evil-quit-all-with-error-code)
  (evil-ex-define-cmd "wqa[ll]" 'evil-save-and-quit)
  (evil-ex-define-cmd "xa[ll]" "wqall")
  (evil-ex-define-cmd "x[it]" 'evil-save-modified-and-close)
  (evil-ex-define-cmd "exi[t]" 'evil-save-modified-and-close)
  (evil-ex-define-cmd "bd[elete]" 'evil-delete-buffer)
  (evil-ex-define-cmd "bw[ipeout]" 'evil-delete-buffer)
  ;; state change
  (evil-ex-define-cmd "g[lobal]" 'evil-ex-global)
  (evil-ex-define-cmd "v[global]" 'evil-ex-global-inverted)
  (evil-ex-define-cmd "norm[al]" 'evil-ex-normal)
  ;; substitution
  (evil-ex-define-cmd "s[ubstitute]" 'evil-ex-substitute)
  (evil-ex-define-cmd "&" 'evil-ex-repeat-substitute)
  (evil-ex-define-cmd "&&" 'evil-ex-repeat-substitute-with-flags)
  (evil-ex-define-cmd "~" 'evil-ex-repeat-substitute-with-search)
  (evil-ex-define-cmd "~&" 'evil-ex-repeat-substitute-with-search-and-flags)
  ;; registers and marks
  (evil-ex-define-cmd "registers" 'evil-show-registers)
  (evil-ex-define-cmd "marks" 'evil-show-marks)
  (evil-ex-define-cmd "delm[arks]" 'evil-delete-marks)
  (evil-ex-define-cmd "ju[mps]" 'evil-show-jumps)
  (evil-ex-define-cmd "noh[lsearch]" 'evil-ex-nohighlight)
  (evil-ex-define-cmd "f[ile]" 'evil-show-file-info)
  ;; shifting / aligning
  (evil-ex-define-cmd "<" 'evil-shift-left)
  (evil-ex-define-cmd ">" 'evil-shift-right)
  ;; print last line number
  (evil-ex-define-cmd "=" 'evil-ex-line-number)
  (evil-ex-define-cmd "!" 'evil-shell-command)
  (evil-ex-define-cmd "@:" 'evil-ex-repeat)
  (evil-ex-define-cmd "mak[e]" 'evil-make)
  ;; errors
  (evil-ex-define-cmd "cc" 'evil-goto-error)
  (evil-ex-define-cmd "cfir[st]" 'first-error)
  (evil-ex-define-cmd "cr[ewind]" 'first-error)
  (evil-ex-define-cmd "cn[ext]" 'next-error)
  (evil-ex-define-cmd "cp[revious]" 'previous-error)
  (evil-ex-define-cmd "set-initial-state" 'evil-ex-set-initial-state)
  (evil-ex-define-cmd "show-digraphs" 'evil-ex-show-digraphs)
  ;;sorting
  (evil-ex-define-cmd "sor[t]" 'evil-ex-sort)
  ;; window resizing
  (evil-ex-define-cmd "res[ize]" 'evil-ex-resize)
  )

(defun jg-spacemacs-main-layer/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
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
  )

(defun jg-spacemacs-main-layer/pre-init-yasnippet ()
  ;; If an error occurs, change yas-installed-snippets-dir is not in yas-snippet-dirs
  ;; as it is obsolete
  (setq yas-snippet-dirs `(,(expand-file-name "~/.spacemacs.d/snippets/")))
  ;; ,(expand-file-name "~/github/otherLibs/yasnippet-snippets/snippets")
  ;; ,(expand-file-name "~/github/otherLibs/yasnippet-snippets")

  )

(defun jg-spacemacs-main-layer/post-init-yasnippet ()
  (spacemacs/declare-prefix "y" "Snippets/Abbevs")
  (spacemacs/set-leader-keys
    "y y"    'yas-expand
    "y i"    'yas-insert-snippet
    "y n"    'yas-new-snippet
    "y d"    'yas-describe-tables
    "y v"    'yas-visit-snippet-file
    )

  (spacemacs/set-leader-keys-for-major-mode 'edit-abbrevs-mode
    "y s" 'abbrev-edit-save-buffer
    )

  (global-set-key (kbd "C-c ;") 'expand-abbrev)
  (global-set-key (kbd "C-c >") 'yas-new-snippet)
  )

(defun jg-spacemacs-main-layer/init-yasnippet-snippets ()
  (use-package yasnippet-snippets
    :init
    (yasnippet-snippets-initialize)
    )
  )

(defun jg-spacemacs-main-layer/post-init-abbrev ()
  ;;abbrev-file complaint quieting
  (setq-default
   abbrev-file-name (expand-file-name "~/.spacemacs.d/setup_files/abbrevs_defs")
   )
  (spacemacs/set-leader-keys
    "y e"  'edit-abbrevs
    "y w"  'write-abbrev-file
    "y r"  'read-abbrev-file
    "y a"  'add-global-abbrev
    "y A"  'add-mode-abbrev
    "y k"  'kill-all-abbrevs
    )
  )

(defun jg-spacemacs-main-layer/pre-init-python ()
  (spacemacs|use-package-add-hook python
    :post-config
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
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "d b" 'jg-spacemacs-main-layer/python-toggle-breakpoint
      )
    (evil-define-key 'normal python-mode-map
      (kbd "z d") 'jg-spacemacs-main-layer/toggle-all-defs
      (kbd "z C") 'jg-spacemacs-main-layer/close-class-defs
      )
    )
  )

(defun jg-spacemacs-main-layer/post-init-flycheck ()
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

(defun jg-spacemacs-main-layer/post-init-origami ()
  (require 'jg-spacemacs-main-layer/origami-python-parser "~/.spacemacs.d/layers/jg-spacemacs-main-layer/local/origami-parser.el")
  (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
  (add-to-list 'origami-parser-alist '(python-mode . jg-spacemacs-main-layer/origami-python-parser))
  )

(defun jg-spacemacs-main-layer/init-vlf ()
  (use-package vlf-setup
    :config (progn
              (define-key evil-normal-state-map (kbd "] A") 'vlf-next-batch-from-point)
              (define-key evil-normal-state-map (kbd "] a") 'vlf-next-batch)
              (define-key evil-normal-state-map (kbd "[ a") 'vlf-prev-batch)
              (spacemacs/set-leader-keys "a U v " 'vlf-set-batch-size))
    )
  )

(defun jg-spacemacs-main-layer/init-evil-quickscope ()
  (use-package evil-quickscope
    :init
    (spacemacs/set-leader-keys
      "t q" 'jg-spacemacs-main-layer/toggle-quickscope-always)
    :config
    (global-evil-quickscope-always-mode 1)
    )
  )

(defun jg-spacemacs-main-layer/pre-init-dired ()
  (spacemacs/set-leader-keys
    "a d" nil
    "K"   nil
    )

  (evil-define-key 'normal dired-mode-map
    (kbd "<DEL>") 'dired-kill-subdir
    (kbd "i")     'jg-spacemacs-main-layer/dired-insert-subdir-maybe-recursive
    )

  (spacemacs|use-package-add-hook dired
    :post-config
    (evil-define-key 'normal dired-mode-map
      (kbd "C-h") nil)
    (evil-define-key* 'normal dired-mode-map
                      (kbd "M-n") 'jg-spacemacs-main-layer/dired-auto-move
                      (kbd "C-h") 'dired-up-directory
                      )
    )
  )

(defun jg-spacemacs-main-layer/post-init-dired-x ()
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  )

(defun jg-spacemacs-main-layer/post-init-dired ()
  (spacemacs/declare-prefix-for-mode 'dired-mode "m d" "Describe")
  (spacemacs/declare-prefix-for-mode 'dired-mode "m K" "Destructive Edits")
  (spacemacs/declare-prefix-for-mode 'dired-mode "m m" "Mark")
  (spacemacs/declare-prefix-for-mode 'dired-mode "m f" "Find")
  (spacemacs/declare-prefix-for-mode 'dired-mode "m g" "Gtags")
  (spacemacs/set-leader-keys-for-major-mode 'dired-mode
    "d s" 'jg-spacemacs-main-layer/dired-create-summary-of-orgs
    "d m" 'jg-spacemacs-main-layer/dired-marked-info
    "d d" 'jg-spacemacs-main-layer/dired-diff
    )
  )

(defun jg-spacemacs-main-layer/post-init-shell-pop ()
  (spacemacs/set-leader-keys
    "as" nil)
  )

(defun jg-spacemacs-main-layer/pre-init-ibuffer ()
  (spacemacs|use-package-add-hook ibuffer
    :post-config
    (setq ibuffer-group-buffers-by nil)
    )

  (add-hook 'ibuffer-hook 'jg-spacemacs-main-layer/setup-ibuffer)
  )

(defun jg-spacemacs-main-layer/init-vimish-fold ()
  (use-package vimish-fold
    :init
    (vimish-fold-global-mode)
    :config
    (evil-define-key '(normal visual) global-map
      (kbd "z v a") 'vimish-fold-toggle
      (kbd "z v A") 'vimish-fold-toggle-all
      (kbd "z v f") 'vimish-fold
      (kbd "z v d") 'vimish-fold-delete
      (kbd "z v D") 'vimish-fold-delete-all
      (kbd "z v j") 'vimish-fold-next-fold
      (kbd "z v k") 'vimish-fold-previous-fold
      (kbd "z v m") 'vimish-fold-refold-all
      (kbd "z v r") 'vimish-fold-unfold-all
      )
    )
  )

(defun jg-spacemacs-main-layer/post-init-hi-lock ()
  (message "Setting up Hi lock")
  (global-hi-lock-mode)

  (evil-define-key '(normal visual) hi-lock-map
    (kbd "z ' i") 'hi-lock-write-interactive-patterns
    (kbd "z ' u") 'unhighlight-regexp
    (kbd "z ' .") 'highlight-symbol-at-point
    (kbd "z ' r") 'highlight-regexp
    (kbd "z ' p") 'highlight-phrase
    (kbd "z ' l") 'highlight-lines-matching-regexp
    (kbd "z ' f") 'hi-lock-find-patterns
    )

  (which-key-add-prefix-title (kbd "z '") "Hi-lock")
  (setq hi-lock-auto-select-face t)
  )

(defun jg-spacemacs-main-layer/post-init-which-key ()
  (defun which-key-toggle-docstrings (&optional _)
    (interactive)
    (jg-spacemacs-main-layer/toggle-docstrings)
    (which-key-reload-key-sequence)
    (which-key--create-buffer-and-show (which-key--current-prefix))
    )
)

(defun jg-spacemacs-main-layer/pre-init-neotree ()
  (spacemacs|use-package-add-hook neotree
    :post-config
    (push "^__pycache__$" neo-hidden-regexp-list)
    (push "^G\\(PATH\\|R?TAGS\\)$" neo-hidden-regexp-list)
    (push "^__init__.py$" neo-hidden-regexp-list)
    )
  )

(defun jg-spacemacs-main-layer/post-init-pyenv-mode ()
  """ Remove the annoying python-shell-setup advice """
  (dolist (func '(pyenv-mode-set pyenv-mode-unset))
    (advice-remove func 'spacemacs/python-setup-shell))

  )

(defun jg-spacemacs-main-layer/post-init-pyvenv ()
  """ Remove the annoying python-shell-setup advice """

  (dolist (func '(pyvenv-activate pyvenv-deactivate pyvenv-workon))
               (advice-remove func 'spacemacs/python-setup-shell))

  (spacemacs/set-leader-keys-for-major-mode 'dired-mode
    "v" 'pyvenv-activate
    )

  )

(defun jg-spacemacs-main-layer/post-init-swiper ()
  (spacemacs/set-leader-keys "pl" nil)
)

(defun jg-spacemacs-main-layer/post-init-dired-quick-sort ()
  (setq dired-quick-sort-group-directories-last ?y)

  )

(defun jg-spacemacs-main-layer/post-init-avy ()
  (setq avy-timeout-seconds 0.2)
  )

(defun jg-spacemacs-main-layer/post-init-helm-gtags ()
  ;; Adapated from helm-gtags layer
  (let ((jumpl (intern (format "spacemacs-jump-handlers-%S" 'python-mode))))
    (when (boundp jumpl)
      (add-to-list jumpl 'spacemacs/helm-gtags-maybe-dwim 'append)))

  (spacemacs/set-leader-keys-for-major-mode 'python-mode
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

(defun jg-spacemacs-main-layer/post-init-evil-iedit-state ()
  ;; Used until reference to function is updated in iedit-done
  (defalias 'iedit-cleanup 'iedit-lib-cleanup)
  )
