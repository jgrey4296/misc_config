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

(spec-handling-new! electric nil :loop 'hook
                    :struct '(:chars list :words list)
                    (setq-local electric-indent-inhibit nil)
                    (electric-indent-local-mode +1)
                    (-when-let (chars (plist-get val :chars))
                      (setq-local electric-indent-chars chars))
                    (-when-let (words (plist-get val :words))
                      (setq +electric-indent-words words))
                    )

(spec-handling-new! flyspell-predicate nil :loop 'hook
                    (setq-local flyspell-generic-check-word-predicate val)
                    )

;; (defvar +jg-format-defs nil)
;; (spec-handling-new! formatting +jg-format-defs
;;                     :loop 'collect
;;                     val
;;                     )


;; TODO


;; (spec-handling-add! format
;;                     '(sh-mode
;;                       '("shfmt" "-ci"
;;                         ("-i" "%d" (unless indent-tabs-mode tab-width))
;;                         ("-ln" "%s" (pcase sh-shell (`bash "bash") (`mksh "mksh") (_ "posix"))))
;;                       )
;;                     )


;; tidy is already defined by the format-all package. We redefine it to add
;; more sensible arguments to the tidy command.
;; (set-formatter! 'html-tidy
;;   '("tidy" "-q" "-indent"
;;     "--tidy-mark" "no"
;;     "--drop-empty-elements" "no"
;;     ("--show-body-only" "%s" (if +format-region-p "true" "auto"))
;;     ("--indent-spaces" "%d" tab-width)
;;     ("--indent-with-tabs" "%s" (if indent-tabs-mode "yes" "no"))
;;     ("-xml" (memq major-mode '(nxml-mode xml-mode))))
;;   :ok-statuses '(0 1))

;; (spec-handling-add! format
;;                     '(caml-mode #'ocamlformat)
;;                     '(tuareg-mode #'ocamlformat)
;;                     )
