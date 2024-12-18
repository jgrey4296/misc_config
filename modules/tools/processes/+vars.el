;;; +vars.el -*- lexical-binding: t; -*-


(speckler-add! popup ()
  `(processes
    ("^\\*PSTree\\*"    :side right :ttl 5 :quit t :width 0.4)
    ("^\\*Launchctl\\*" :side right :ttl 5 :quit t :width 0.4)
    )
  )
