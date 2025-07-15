;;; transient-guides.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'display-fill-column-indicator)
(require 'macro-tools--transient)
(require 'glasses)

;; Guides
(transient-toggle-hook! fill-column-indicator ()
  ""
  :hook 'prog-mode
  :fn #'display-fill-column-indicator-mode
  :global t
  :desc (format "Columns : %-3s" fill-column)
  :key "c"
  )
(transient-toggle-hook! indent-guides ()
  "Indents"
  :hook 'prog-mode
  :fn #'highlight-indent-guides-mode
  :global t
  :key "i"
  )
(transient-toggle-hook! display-line-numbers ()
  "Line Numbers"
  :key "n"
  :global t
  :hook '(prog-mode text-mode)
  :fn #'display-line-numbers-mode
  )
(transient-toggle-hook! glasses ()
  "Glasses"
  :key "g"
  :global t
  :hook '(prog-mode text-mode)
  :fn #'glasses-mode
  )
(transient-toggle-mode! ruler-mode ()
  "Ruler"
  :key "r"
  )
(transient-toggle-mode! whitespace-mode  ()
  "Whitespace"
  :key "w"
  )

(transient-call! spelling ()
  "Toggle flyspell, writegood, spell-fu"
  :key "s"
  :desc (transient-mode-fmt "Spelling" flyspell-mode "s")
  (flyspell-mode 'toggle)
  (writegood-mode (if flyspell-mode 2 -1))
  (spell-fu-mode (if flyspell-mode 2 -1))
  )

;;;###autoload
(defun +jg-ui-build-guides-transient ()
  (transient-subgroup! jg-toggle-guides-transient ()
    "For controlling ui guide settings"
    :key "g"
    :desc "|| Guides     ||"
    [
     (transient-macro-toggle-hook-fill-column-indicator)
     (transient-macro-toggle-hook-indent-guides)
     (transient-macro-toggle-hook-display-line-numbers)
     (transient-macro-toggle-hook-glasses)
     ]
    [
     (transient-macro-toggle-ruler-mode)
     (transient-macro-toggle-whitespace-mode)
     (transient-macro-call-spelling)
     ]
    )

  (transient-guarded-insert-subgroup! 'jg-toggle-main jg-toggle-guides-transient (1 -1))

  (transient-append-suffix 'jg-toggle-main "E" '("W" transient-macro-call-spelling))
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
;;; transient-guides.el ends here
