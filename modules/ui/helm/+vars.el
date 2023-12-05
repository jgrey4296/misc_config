;;; completion/ivy/+vars.el -*- lexical-binding: t; -*-

;;-- personal-vars
(defvar jg-completion-rps-have-you-played- (expand-file-name "~/github/bibliography/plus/urls/have-you-playeds"))
;;-- end personal-vars

;;-- helm
(setq  helm-candidate-number-limit 50
       helm-truncate-lines t
       ;; Remove extraineous helm UI elements
       helm-display-header-line nil
       helm-mode-line-string nil
       helm-ff-auto-update-initial-value nil
       helm-find-files-doc-header nil
       ;; Default helm window sizes
       helm-display-buffer-default-width nil
       helm-display-buffer-default-height 0.25
       ;; When calling `helm-semantic-or-imenu', don't immediately jump to
       ;; symbol at point
       helm-imenu-execute-action-at-once-if-one nil
       ;; disable special behavior for left/right, M-left/right keys.
       helm-ff-lynx-style-map nil
       ;; Matching
       ;; helm-apropos-fuzzy-match     fuzzy
       ;; helm-bookmark-show-location  fuzzy
       ;; helm-buffers-fuzzy-matching  fuzzy
       ;; helm-ff-fuzzy-matching       fuzzy
       ;; helm-file-cache-fuzzy-match  fuzzy
       ;; helm-flx-for-helm-locate     fuzzy
       ;; helm-imenu-fuzzy-match       fuzzy
       ;; helm-lisp-fuzzy-completion   fuzzy
       ;; helm-locate-fuzzy-match      fuzzy
       ;; helm-projectile-fuzzy-match  fuzzy
       ;; helm-recentf-fuzzy-match     fuzzy
       ;; helm-semantic-fuzzy-match    fuzzy
       )

(setq helm-boring-file-regexp-list (list (rx "." (|
                                            "projects" "DS_Store"
                                            "cm" "cmti" "cmt" "annot" "cmi" "cmxa" "cma" "cmx" "cmo" "beam" "vee" "jam" "hi" "pho" "phi" "glob" "vo" "o" "~" "bin"
                                            "lbin" "so" "a" "ln" "blg" "bbl" "elc" "lof" "glo" "idx" "lot" "svn" "hg" "git" "bzr" "CVS" "_darcs" "_MTN" "fmt" "tfm" "class" "fas" "lib" "mem"
                                            "x86f" "sparcf" "dfsl" "pfsl" "d64fsl" "p64fsl" "lx64fsl" "lx32fsl" "dx64fsl" "dx32fsl" "fx64fsl" "fx32fsl" "sx64fsl" "sx32fsl" "wx64fsl" "wx32fsl" "fasl" "ufsl" "fsl" "dxl" "lo" "la" "gmo" "mo" "toc" "aux" "cp" "fn" "ky" "pg" "tp" "vr" "cps" "fns"
                                            "kys" "pgs" "tps" "vrs" "pyc" "pyo")
                                       eol ))
       helm-find-files-actions '(
                                 ("Change mode on file(s) `M-M'"                                    . helm-ff-chmod)
                                 ("Complete at point `C-c i'"                                       . helm-insert-file-name-completion-at-point)
                                 ("Copy file to dir(s) `C-M-c'"                                     . helm-ff-mcp)
                                 ("Copy file(s) `M-C, C-u to follow'"                               . helm-find-files-copy)
                                 ("Delete File(s) `M-D' (C-u reverse trash)"                        . helm-ff-delete-files)
                                 ("Describe Random"                                                 . +jg-completion-helm-describe-random-action)
                                 ("Find File"                                                       . helm-find-file-or-marked)
                                 ("Find file as root `C-c r'"                                       . helm-find-file-as-root)
                                 ("Find file in Dired"                                              . helm-point-file-in-dired)
                                 ("Find file in hex dump"                                           . hexl-find-file)
                                 ("Open Random External"                                            . +jg-completion-helm-open-random-external-action)
                                 ("Open Random"                                                     . +jg-completion-helm-open-random-action)
                                 ("Open file externally `C-c C-x, C-u to choose'"                   . helm-open-file-externally)
                                 ("Rename file(s) `M-R, C-u to follow'"                             . helm-find-files-rename)
                                 ("Touch File(s) `M-T'"                                             . helm-ff-touch-files)
                                 ("View file"                                                       . view-file)
                                 )
       )
;;-- end helm
