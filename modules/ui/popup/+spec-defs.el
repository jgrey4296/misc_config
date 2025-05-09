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

(speckler-new! popup (key val)
  "Specify popup buffer rules.
Modifies the display-buffer-alist rules
"
  :target display-buffer-alist
  :sorted t
  :loop 'append
  :struct '(regexp :size :side :width :height :slot :vslot :ttl :quit :select :modeline :autosave)
  (cl-loop for rule in val
           collect
           (let* ((regexp (car rule))
                  (plist (cdr rule)) ;; (append (cdr rule) +popup-defaults))
                  (priority (* -1 (or (plist-get (cdr rule) :priority) 0)))
                  )
             (if (plist-get plist :ignore)
                 `(,priority ,regexp ())
               ;; Build the alist spec
               `(,priority ,regexp +popup-buffer
                 ;; Buffer params
                 (actions       . ,(upfun! (plist-get plist :actions)))
                 (side          . ,(upfun! (plist-get plist :side)))
                 (size          . ,(upfun! (plist-get plist :size)))
                 (window-width  . ,(upfun! (plist-get plist :width)))
                 (window-height . ,(upfun! (plist-get plist :height)))
                 (slot          . ,(upfun! (plist-get plist :slot)))
                 (vslot         . ,(upfun! (plist-get plist :vslot)))
                 ;; window-parameters
                 (window-parameters
                  (ttl      . ,(upfun! (plist-get plist :ttl)))
                  (quit     . ,(upfun! (plist-get plist :quit)))
                  (select   . ,(upfun! (plist-get plist :select)))
                  (modeline . ,(upfun! (plist-get plist :modeline)))
                  (autosave . ,(upfun! (plist-get plist :autosave)))
                  ,@(plist-get plist :parameters))
                 )
               )
             )
           )
  )
