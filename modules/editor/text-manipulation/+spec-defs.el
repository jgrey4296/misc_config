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
                    )
