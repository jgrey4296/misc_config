;;; treesitter.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun python-ts-extend (&rest args)
  "TODO use treesit-add-font-lock-rules"
  (setq-local treesit-font-lock-feature-list '(;; 1
                                               ( comment definition keyword string type)
                                               ;; 2
                                               ( public constant builtin errors typealias
                                                        assignment operator variable
                                                        )
                                               ;; 3
                                               ( decorator
                                                 escape-sequence
                                                 number
                                                 string-interpolation
                                                 return
                                                 internal
                                                 function
                                                 wrappers
                                                 )
                                               ;; 4
                                               ( bracket
                                                 delimiter
                                                 property
                                                 conventions
                                                 )
                                               )
              ;; Rules
              treesit-font-lock-settings (append
                                          treesit-font-lock-settings
                                          (treesit-font-lock-rules
                                           :feature 'typealias
                                           :language 'python
                                           :override 'append
                                           '((type_alias_statement "type" @jg-unimportant))

                                           :feature 'public
                                           :language 'python
                                           :override 'append
                                           '((function_definition
                                              (identifier) @jg-public
                                              (:match "^[^_]." @jg-public)
                                              ))

                                           :feature 'internal
                                           :language 'python
                                           :override t
                                           '((function_definition
                                              (identifier) @jg-internal
                                              (:match "^_[^_]." @jg-internal))
                                             (function_definition
                                              (identifier) @jg-dunder
                                              (:match "^__.+?__\\'" @jg-dunder))
                                             )

                                           :feature 'errors
                                           :language 'python
                                           :override 'append
                                           '(((identifier) @jg-error
                                              (:match "^.*?E\\(xception\\|rror\\)\\'" @jg-error)
                                              ))

                                           :feature 'conventions
                                           :language 'python
                                           :override t
                                           '(((identifier) @jg-convention
                                              (:match ".+?_[pdisccefmhl]\\'" @jg-convention)
                                              ))

                                           :feature 'return
                                           :language 'python
                                           :override 'append
                                           '((block)
                                             ((return_statement "return" @jg-return
                                                                (:match "^\s+return" @jg-return)
                                                                ))
                                             )

                                           :feature 'wrappers
                                           :language 'python
                                           :override 'append
                                           '((dictionary ["{" "}"] @jg-dict)
                                             (list ["[" "]"] @jg-list)
                                             (argument_list ["(" ")"] @jg-args)
                                             )
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
