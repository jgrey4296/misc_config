;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! auto-modes
                    '(fortran
                      ("\\.F90" . f90-mode)
                      ("\\.FOR$" . fortran-mode)
                      )
                    )

(spec-handling-add! popup
                    '(fortran
                      ("^\\*fortran-compilation" :side 'right :size 0.5 :quit t)
                      ( "^\\*fortran-compilation" :side 'right :size 0.5 :quit t)
                      )
                    )
