;;; +evil-ex.el -*- lexical-binding: t; -*-

(evil-ex-define-cmd "gbrowse"     #'+vc/browse-at-remote) ; show file/region in github/gitlab
(evil-ex-define-cmd "gissues"     #'forge-browse-issues)  ; show github issues
(evil-ex-define-cmd "git"         #'magit-status)         ; open magit status window
(evil-ex-define-cmd "gstage"      #'magit-stage)
(evil-ex-define-cmd "gunstage"    #'magit-unstage)
(evil-ex-define-cmd "gblame"      #'magit-blame)
(evil-ex-define-cmd "grevert"     #'git-gutter:revert-hunk)
