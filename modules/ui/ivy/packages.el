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

(package! flx)

(package! ivy-posframe)

(package! general-insert :recipe (:host github :repo "jgrey4296/misc-modes" :files ("minor-modes/general-insert/*.el") :local-repo "misc-modes"))
