;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper)
(package! ivy)
(package! ivy-hydra)
(package! ivy-avy)
(package! ivy-rich)
(package! counsel)

(package! amx)
(package! counsel-projectile)
(package! wgrep)

(package! ivy-prescient)
(package! flx)

(package! ivy-posframe)

(package! general-insert :recipe `(:local-repo ,(expand-file-name "packages/minor-modes/general-insert" doom-user-dir)))
