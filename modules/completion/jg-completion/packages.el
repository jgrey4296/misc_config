;; -*- no-byte-compile: t; -*-
;;; completion/ivy/packages.el

(package! swiper )
(package! ivy)
(package! ivy-hydra)
(package! counsel)
(package! cl-lib :built-in t)
(package! amx )
(package! counsel-projectile )
(package! ivy-rich )
(package! wgrep )

(if (featurep! +prescient)
    (package! ivy-prescient )
  (when (featurep! +fuzzy)
    (package! flx )))

(when (featurep! +childframe)
  (package! ivy-posframe ))

(when (featurep! +icons)
  (package! all-the-icons-ivy ))
