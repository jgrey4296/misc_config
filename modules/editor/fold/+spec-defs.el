;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new! fold (key val)
  "Registers fold handlers"
  :target evil-fold-list
  :sorted t
  :loop 'collect
  :struct '(:modes list :priority int :triggers
            (:delete fn :open-all fn :close-all-fn :toggle fn :open fn :open-rec fn :close fn))
  (append (list (* -1 (or (plist-get val :priority) 0)))
          (list (ensure-list (plist-get val :modes)))
          (cl-loop for (kwd . fn) in (map-pairs (plist-get val :triggers))
                   collect kwd
                   collect (spec-handling-unquote! fn))
          )
  )

(speckler-new! hideshow (key val)
  "Set hide show special modes"
  :struct '(list (mode "start" "end" "comment-start"...))
  :target hs-special-modes-alist
  :loop 'append
  val
  )
