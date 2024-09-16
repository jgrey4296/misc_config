;;; transient-nav.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;; Nav
(progn
  (transient-make-mode-toggle! centered-cursor-mode   "Center Cursor" "c")
  (transient-make-mode-toggle! minimap-mode           "Minimap"       "m")
  (transient-make-mode-toggle! evil-visual-mark-mode  "visual mark"   "v")

  ;; TODO add sidebar selection
  (transient-make-call! sidebar "s" "Sidebar"    (+jg-ui-tree/open))
  (transient-make-call! frame-fullscreen "f" "Fullscreen" (toggle-frame-fullscreen))


  )

;;;###autoload
(defun +jg-ui-build-nav-transient ()
  (transient-make-subgroup! jg-toggle-nav-transient "n"
                            "For controlling ui nav settings"
                            :desc "|| Navigation ||"
                            [:description "|| Navigation ||"
                                          [
                                           (transient-macro-toggle-centered-cursor-mode)
                                           (transient-macro-toggle-minimap-mode)
                                           (transient-macro-toggle-evil-visual-mark-mode)
                                           ]
                                          [
                                           (transient-macro-call-sidebar)
                                           (transient-macro-call-frame-fullscreen)
                                           ]
                                          []
                                          ]
                            )

  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    August 22, 2024
;; Modified:   August 22, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; transient-nav.el ends here
