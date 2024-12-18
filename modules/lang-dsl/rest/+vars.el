;;; +vars.el -*- lexical-binding: t; -*-

(speckler-add! auto-modes ()
  '(rest
    ("\\.http\\'" . restclient-mode)
    )
  )

(speckler-add! popup ()
  '(rest
    ("^\\*HTTP Response" :size 0.4 :quit 'other)
    )
  )


(speckler-add! company ()
  '(restclient-mode (:mode company-restclient))
  )
