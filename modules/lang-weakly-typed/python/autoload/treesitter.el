;;; treesitter.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defun python-ts-extend (&rest args)
  (setq-local treesit-font-lock-feature-list '(( comment definition)
                                               ( keyword string type)
                                               ( assignment builtin constant decorator escape-sequence number string-interpolation typealias return dunder internal)
                                               ( bracket delimiter function operator variable property )
                                               )
              treesit-font-lock-settings (append
                                          treesit-font-lock-settings
                                          (treesit-font-lock-rules
                                           :feature 'typealias
                                           :language 'python
                                           :override t
                                           '((type_alias_statement "type" @font-lock-semi-unimportant))

                                           :feature 'public
                                           :language 'python
                                           :override t
                                           '((function_definition
                                              (identifier) @ediff-fine-diff-Ancestor
                                              (:match "^[^_]." @ediff-fine-diff-Ancestor)
                                             ))


                                           :feature 'internal
                                           :language 'python
                                           :override t
                                           '((function_definition
                                              (identifier) @jg-replace-line
                                              (:match "^_[^_]." @jg-replace-line)
                                             ))

                                           :feature 'dunder
                                           :language 'python
                                           :override t
                                           '((function_definition
                                             (identifier) @jg-motion-line
                                             (:match "^__.+?__$" @jg-motion-line)
                                             ))

                                          ;; :feature 'return
                                          ;; :language 'python
                                          ;; :override t
                                          ;; '((block)
                                          ;;   ((return_statement "return" @font-lock-semi-unimportant
                                          ;;                      (:match "^\s+return" @font-lock-semi-unimportant)
                                          ;;                      ))
                                          ;;   )
                                          )
              )
  )
)


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 06, 2025
;; Modified:   January 06, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; treesitter.el ends here
