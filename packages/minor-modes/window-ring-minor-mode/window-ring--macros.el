;;; window-ring--macros.el -*- lexical-binding: t; -*-

(defmacro with-window-ring (&rest body)
  (declare (indent 1))
  `(when (persp-parameter 'window-ring)
     (let ((wr-persp      (get-current-persp))
           (wr-actual     (persp-parameter 'window-ring-actual))
           (wr-grow       (persp-parameter 'window-ring-grow))
           (wr-loop       (persp-parameter 'window-ring-loop))
           (wr-duplicates (persp-parameter 'window-ring-duplicates))
           (wr-focus      (persp-parameter 'window-ring-focus))
           (wr-max        (persp-parameter 'window-ring-max))
           (wr-scratch    (persp-parameter 'window-ring-scratch))
           )
       ,@body
       )
     )
  )

(defmacro with-other-window-ring (persp &rest body)
  (declare (indent 1))
  `(when (persp-parameter 'window-ring persp)
     (let ((wr-persp      persp)
           (wr-actual     (persp-parameter 'window-ring-actual persp))
           (wr-grow       (persp-parameter 'window-ring-grow persp))
           (wr-loop       (persp-parameter 'window-ring-loop persp))
           (wr-duplicates (persp-parameter 'window-ring-duplicates persp))
           (wr-focus      (persp-parameter 'window-ring-focus persp))
           (wr-max        (persp-parameter 'window-ring-max persp))
           )
       ,@body
       )
     )
  )

(defmacro when-window-ring (&rest body)
  (declare (indent 1))
  `(when (persp-parameter 'window-ring)
     ,@body
     )
  )

(defmacro with-window-ring-adding (&rest body)
  `(let ((window-ring--adding t))
     ,@body
     )
  )


(provide 'window-ring--macros)
