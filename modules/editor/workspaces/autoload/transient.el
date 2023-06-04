;;; transient.el -*- lexical-binding: t; -*-
(require 'transient)

(progn
  (transient-make-toggle! hide-mode-line-mode)
  (transient-make-call!   quickscope
                          (format "")

                          )
  )

(transient-define-prefix jg-transient-workspace ()
  ""
  []
  [""
   ("q" "Quit" transient-quit-one)
   ]
  )

(transient-define-prefix jg-transient-window-ring ()
  ""
  []
  [""
   ("q" "Quit" transient-quit-one)
   ]
  )
