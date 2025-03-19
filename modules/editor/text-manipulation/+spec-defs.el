;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new-hook! rotate-text (key val)
  "Set local rotate text lists "
  :struct '(:symbols (list) :words (list) :patterns (list))
  (setq-local rotate-text-local-symbols    (plist-get val :symbols)
              rotate-text-local-words      (plist-get val :words)
              rotate-text-local-patterns   (plist-get val :patterns)
              )
  )

(speckler-new-hook! whitespace-cleanup (key val)
  "Register local whitespace cleanup functions"
  :struct '(list fn)
  (setq-local jg-text-whitespace-clean-hook
              (mapcar #'upfun! (ensure-list val)))
  )

(speckler-new-hook! ligatures (key val)
  "Register ligatures for prettify-symbols"
  :struct '(list)
  (setq-local prettify-symbols-alist
              (let (head alist)
                (while val
                  (setq head (pop val))
                  (pcase (pop val)
                    ((and c (guard (characterp c)))
                     (push (cons head c) alist))
                    ((and c (guard (keywordp c)) (let l (plist-get +ligatures-extra-symbols c)) (guard l))
                     (push (cons head l) alist))
                    )
                  )
                alist
                )
              )
  )

(speckler-new-hook! electric (key val)
  "Register electric chars"
  :struct '(:chars list :words list)
  (setq-local electric-indent-inhibit nil)
  (electric-indent-local-mode +1)
  (-when-let (chars (plist-get val :chars))
    (setq-local electric-indent-chars chars))
  (-when-let (words (plist-get val :words))
    (setq +electric-indent-words words))
  )

(speckler-new-hook! flyspell-predicate (key val)
  "Set local flyspec checkers"
  (setq-local flyspell-generic-check-word-predicate (upfun! val))
  )
