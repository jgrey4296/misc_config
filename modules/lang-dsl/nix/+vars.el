;;; +vars.el -*- lexical-binding: t; -*-

(speckler-add! company ()
  '(nix-mode (:mode . company-nixos-options))
  )
(speckler-add! lookup-handler ()
  '(nix-mode
    :documentation (+nix/lookup-option :async t)
    )
  )
(speckler-add! popup ()
  '(nix
    ("^\\*nixos-options-doc\\*$" :ttl 0 :quit t)
    )
  )
(speckler-add! tree-sit-lang ()
  '(nix-mode . nix)
  )
(speckler-add! auto-modes ()
  '(nix
    ("\\.nix\\'" . nix-mode)
    )
  )

(speckler-add! repl ()
  '(nix-mode :start +nix/open-repl)
  )
