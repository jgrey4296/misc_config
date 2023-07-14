;;; +spec-defs.el -*- lexical-binding: t; -*-

(defvar +jg-format-defs nil)

;; TODO
(spec-handling-new! formatting +jg-format-defs
                    :loop 'collect
                    val
                    )


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
