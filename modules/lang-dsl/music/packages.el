;; sclang packages.el
;; loads second
;; (package! sclang           :recipe `(:local-repo ,(expand-file-name "~/github/supercollider/editors/sc-el/el")))
;; (package! chuck-mode       :recipe `(:local-repo ,(expand-file-name "packages/chuck-mode" doom-user-dir)))
(package! tidal)
(package! music-minor-mode :recipe `(:host github :repo "jgrey4296/misc-modes" :files ("minor-modes/music-minor-mode/*.el")))
(package! csound-mode)
(package! faustine)
