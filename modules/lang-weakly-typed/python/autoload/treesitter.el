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
                                              (:match "^_[^_]." @jg-replace-line))
                                             ;; (function_definition
                                             ;;  (identifier) @jg-emacs-line
                                             ;;  (:match "^__.+?__\\'" @jg-emacs-line))
                                             )

                                           :feature 'errors
                                           :language 'python
                                           :override t
                                           '(((identifier) @jg-motion-line
                                              (:match "^.*?E\\(xception\\|rror\\)\\'" @jg-motion-line)
                                              ))

                                           :feature 'conventions
                                           :language 'python
                                           :override t
                                           '(((identifier) @jg-lisp-line
                                              (:match ".+?_[pdisccefmhl]\\'" @jg-lisp-line)
                                              ))

                                          :feature 'return
                                          :language 'python
                                          :override t
                                          '((block)
                                            ((return_statement "return" @font-lock-semi-unimportant
                                                               (:match "^\s+return" @font-lock-semi-unimportant)
                                                               ))
                                            )

                                          :feature 'wrappers
                                          :language 'python
                                          :override t
                                          '((dictionary ["{" "}"] @glyphless-char)
                                            (list ["[" "]"] @homoglyph)
                                            (argument_list ["(" ")"] @org-link)
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
