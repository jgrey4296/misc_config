;;; +vars.el -*- lexical-binding: t; -*-

(speckler-add! auto-modes ()
  '(fortran
    ("\\.F90" . f90-mode)
    ("\\.FOR$" . fortran-mode)
    )
  )

(speckler-add! popup ()
  '(fortran
    ("^\\*fortran-compilation" :side 'right :size 0.5 :quit t)
    ( "^\\*fortran-compilation" :side 'right :size 0.5 :quit t)
    )
  )
