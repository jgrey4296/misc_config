;; sclang packages.el
;; loads second
(package! sclang           :recipe `(:local-repo ,(expand-file-name "~/github/supercollider/editors/sc-el/el")))
(package! tidal)
;; (package! chuck-mode       :recipe `(:local-repo ,(expand-file-name "packages/chuck-mode" doom-user-dir)))
(package! music-minor-mode :recipe `(:local-repo ,(expand-file-name "packages/music-minor-mode" doom-user-dir)))
(package! csound-mode)
