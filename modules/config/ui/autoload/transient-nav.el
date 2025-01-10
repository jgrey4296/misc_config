;;; transient-nav.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'macro-tools--transient)

;; Nav
(transient-toggle-mode! centered-cursor-mode ()
  "Center Cursor"
  :key "c"
  )
(transient-toggle-mode! minimap-mode ()
  "Minimap"
  :key "m"
  )
(transient-toggle-mode! evil-visual-mark-mode ()
  "visual mark"
  :key "v"
  )
(transient-toggle-hook! cursor-sensor ()
  "For using code-shy's cursor opening/closing"
  :key "c"
  :hook 'code-shy-minor-mode-hook
  :fn #'cursor-sensor-mode
  )

(transient-call! sidebar ()
  "Sidebar"
  :key "s"
  :transient nil
  (+jg-ui-tree/open)
  )
(transient-call! frame-fullscreen ()
  "Fullscree"
  :key "f"
  (toggle-frame-fullscreen)
  )


;;;###autoload
(defun +jg-ui-build-nav-transient ()
  (transient-subgroup! jg-toggle-nav-transient ()
    "For controlling ui nav settings"
    :key "n"
    :desc "|| Navigation ||"
    [
     (transient-macro-toggle-centered-cursor-mode)
     (transient-macro-toggle-minimap-mode)
     (transient-macro-toggle-evil-visual-mark-mode)
     ]
    [
     (transient-macro-call-sidebar)
     (transient-macro-call-frame-fullscreen)
     ]
    [
     (transient-macro-toggle-hook-cursor-sensor)
     ]
    )

  (transient-guarded-insert! 'jg-toggle-main jg-toggle-nav-transient (1 -1))
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
