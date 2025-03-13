;;; transient-visuals.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'elide-head)
(require 'macro-tools--transient)

;; Visual
(transient-toggle-mode! highlight-changes-visible-mode ()
  "Highlight-changes"
  :key "x"
  )
(transient-toggle-mode! reveal-mode ()
  "Reveal Invisible"
  :key "I"
  )
;; Hooks
(transient-toggle-hook! pretty-symbols ()
  "Pretty Symbols"
  :key "p"
  :global t
  :hook 'prog-mode
  :fn #'prettify-symbols-mode
  )
(transient-toggle-hook! rainbow ()
  "Rainbow Colours"
  :key "r"
  :global t
  :hook '(prog-mode text-mode)
  :fn #'rainbow-mode
  )
(transient-toggle-hook! hl-line ()
  "Hl-line"
  :key "h"
  :global t
  :hook '(prog-mode text-mode)
  :fn #'hl-line-mode
  )
(transient-toggle-hook! quickscope ()
  "Quickscope"
  :key "s"
  :global t
  :hook '(prog-mode text-mode)
  :fn #'evil-quickscope-mode
  )
(transient-toggle-hook! highlight-symbol ()
  "Auto Highlight Symbol"
  :key "H"
  :global t
  :hook 'prog-mode
  :fn #'auto-highlight-symbol-mode
  )
(transient-toggle-hook! evil-goggles ()
  "Evil-goggles"
  :key "g"
  :global t
  :hook '(prog-mode text-mode)
  :fn #'evil-goggles-mode
  )
(transient-toggle-hook! highlight-parentheses ()
  "Higlight Parens"
  :key "w"
  :global t
  :hook 'prog-mode
  :fn #'highlight-parentheses-mode
  )

;;
(transient-call! elide-head ()
  "Elide Head"
  :key "e"
  :desc (transient-mode-fmt "Elide Head" elide-head-mode "e")
  (elide-head-mode 'toggle)
  )

(transient-call! link-display ()
  "Link Display"
  :key "l"
  :desc (transient-mode-fmt "LinkDisplay" org-link-descriptive "l")
  (org-toggle-link-display)
  )
(transient-toggle-var! invisible ()
  "Invisible Spec"
  :var line-move-ignore-invisible
  :key "i"
  )

;;;###autoload
(defun +jg-ui-build-visuals-transient ()
  (transient-subgroup! jg-toggle-visuals-transient ()
    "For controlling ui visual settings"
    :key "v"
    :desc "|| Visuals    ||"
    [
     (transient-macro-toggle-hook-pretty-symbols)
     (transient-macro-toggle-hook-rainbow)
     (transient-macro-toggle-hook-hl-line)
     (transient-macro-toggle-hook-quickscope)
     ]
    [
     (transient-macro-toggle-hook-highlight-symbol)
     (transient-macro-toggle-hook-evil-goggles)
     (transient-macro-toggle-hook-highlight-parentheses)
     ]
    [
     (transient-macro-toggle-invisible)
     (transient-macro-call-link-display)
     (transient-macro-toggle-highlight-changes-visible-mode)
     (transient-macro-call-elide-head)
     ]
  )

  (transient-guarded-insert! 'jg-toggle-main jg-toggle-visuals-transient (1 -1))
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
;;; transient-visuals.el ends here
