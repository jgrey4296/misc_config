;;; lang/coq/config.el -*- lexical-binding: t; -*-

(when (modulep! +coq) (local-load! "+coq"))
(when (modulep! +agda) (local-load! "+agda"))
(when (modulep! +idris) (local-load! "+idris"))
(when (modulep! +fstar) (local-load! "+fstar"))
(when (modulep! +lean) (local-load! "+lean"))
(defer-load! (jg-bindings-total) "+bindings")

(use-package! proof-general
  :init
  (setq proof-splash-enable nil)
  :config
  (after! proof-faces
    (set-face-attribute 'proof-locked-face nil
                        :inverse-video nil
                        :underline t
                        )
    )
  (setq proof-splash-enable nil
        proof-three-window-enable t
        )
  )
