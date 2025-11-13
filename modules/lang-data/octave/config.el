;;; config.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defer-load! jg-bindings-total "+bindings")

(use-package! octave
  :config

  (add-hook! 'octave-mode-hook
             #'abbrev-mode
             #'librarian-insert-minor-mode
             )

  )

(speckler-add! auto-modes ()
  '(octave
    ("\\.m\\'" . octave-mode)
    )
  )

;;; config.el ends here
