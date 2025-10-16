;; packages.el -*- mode: elisp; lexical-binding: t; -*-

(package! music-minor-mode :recipe `(:host github :repo "jgrey4296/misc-modes" :files ("minor-modes/music-minor-mode/*.el")))
(package! tidal)
(package! csound-mode)
(package! faustine)
(package! sclang-extensions)
(package! sclang
  :recipe `(:local-repo ,(expand-file-name "~/github/__libs/music/scel/el")))
;; (package! chuck-mode
;;   :recipe `(:local-repo ,(expand-file-name "packages/chuck-mode" doom-user-dir)))
