;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! rotate-text nil :loop 'hook
                    (setq-local rotate-text-local-symbols    (plist-get val :symbols)
                                rotate-text-local-words      (plist-get val :words)
                                rotate-text-local-patterns   (plist-get val :patterns)
                                )
                    )

(spec-handling-new! whitespace-cleanup nil :loop 'hook
                    (setq-local jg-text-whitespace-clean-hook (ensure-list val))
                    )

(spec-handling-new! ligatures nil :loop 'hook
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
                    (prettify-symbols-mode)
                    )

(spec-handling-new! electric nil :loop 'hook
                    :struct '(:chars list :words list)
                    (setq-local electric-indent-inhibit nil)
                    (electric-indent-local-mode +1)
                    (-when-let (chars (plist-get val :chars))
                      (setq-local electric-indent-chars chars))
                    (-when-let (words (plist-get val :words))
                      (setq +electric-indent-words words))
                    )
