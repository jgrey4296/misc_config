;;; +spec-defs.el -*- lexical-binding: t; -*-

(defvar +popup-defaults
  (list :side   'bottom :height 0.16 :width  40 :quit   t :select #'ignore :ttl    5)
  "Default properties for popup rules")

(defvar +popup-defaults-alist
  '(+popup-buffer
    (side . bottom)
    (window-height . 0.16)
    (window-width . 40)
    (window-parameters
     (quit . t)
     (select . ignore)
     (ttl . 5)
     )))

(defvar window-control-popup-persist-default-alist
  '(+popup-buffer
    (side . bottom)
    (window-height . 0.3)

    (window-parameters
     (quit . t)
     (select . nil)
     (modeline . t)
     (ttl . nil)))
  "Popup rule for a persistent buffer")

(spec-handling-new! popup display-buffer-alist
                    :sorted t
                    :loop 'append
                    :doc "Specify popup buffer rules. Each plist is of :struct"
                    :struct '(regexp :size :side :width :height :slot :vslot :ttl :quit :select :modeline :autosave)
                    (cl-loop for rule in val
                             collect
                             (let* ((regexp (car rule))
                                    (plist (append (cdr rule) +popup-defaults))
                                    (priority (* -1 (or (plist-get (cdr rule) :priority) 0)))
                                    )
                               (if (plist-get plist :ignore)
                                   `(,priority ,regexp ())
                                 `(,priority ,regexp +popup-buffer
                                   ;; Buffer params
                                   (actions       . ,(plist-get plist :actions))
                                   (side          . ,(plist-get plist :side))
                                   (size          . ,(plist-get plist :size))
                                   (window-width  . ,(plist-get plist :width))
                                   (window-height . ,(plist-get plist :height))
                                   (slot          . ,(plist-get plist :slot))
                                   (vslot         . ,(plist-get plist :vslot))
                                   ;; window-parameters
                                   (window-parameters
                                    (ttl      . ,(plist-get plist :ttl))
                                    (quit     . ,(plist-get plist :quit))
                                    (select   . ,(plist-get plist :select))
                                    (modeline . ,(plist-get plist :modeline))
                                    (autosave . ,(plist-get plist :autosave))
                                    ,@(plist-get plist :parameters))
                                   )
                                 )
                               )
                             )
                    )
