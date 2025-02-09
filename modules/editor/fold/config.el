;;; editor/fold/config.el -*- lexical-binding: t; -*-

;;
;; Packages

(defconst fold-modes '(vimish-fold-mode
                       hs-minor-mode
                       origami-mode
                       outline-minor-mode
                       hide-ifdef-mode
                       )
  )

(defer-load! jg-bindings-total "+bindings")
(when (modulep! +hideshow) (local-load! "+hideshow"))
(when (modulep! +vimish)   (local-load! "+vimish"))
(when (modulep! +origami)  (local-load! "+origami"))
(when (modulep! +shy)      (local-load! "+shy"))
(when (modulep! +outline)  (local-load! "+outline"))

(speckler-new! fold (key val)
  "Registers fold handlers"
  :target evil-fold-list
  :override nil
  :sorted t
  :loop 'collect
  :struct '(:modes list :priority int :triggers
            (:delete fn :open-all fn :close-all-fn :toggle fn :open fn :open-rec fn :close fn))
  (append (list (* -1 (or (plist-get val :priority) 0)))
          (list (ensure-list (plist-get val :modes)))
          (cl-loop for (kwd . fn) in (map-pairs (plist-get val :triggers))
                   for realfn = (upfun! fn)
                   if (or (null realfn) (functionp realfn))
                   collect kwd
                   and
                   collect realfn
                   )
          )
  )

(speckler-add! fold ()
  '(ifdef
    :modes (hide-ifdef-mode)
    :priority -25
    :triggers (:open-all   #'show-ifdefs
               :close-all  #'hide-ifdefs
               :toggle     nil
               :open       #'show-ifdef-block
               :open-rec   nil
               :close      #'hide-ifdef-block
               )
    )
  )
