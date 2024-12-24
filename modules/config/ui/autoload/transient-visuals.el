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
(transient-toggle-mode! global-prettify-symbols-mode ()
  "Pretty Symbols"
  :key "p"
  )
(transient-toggle-mode! hl-line-mode ()
  "Highlight-line"
  :key "h"
  )
(transient-toggle-mode! evil-goggles-mode ()
  "Evil-goggles"
  :key "g"
  )
(transient-toggle-mode! highlight-parentheses-mode ()
  "Higlight-wrappers"
  :key "w"
  )
(transient-toggle-mode! rainbow-mode ()
  "Rainbow Colours"
  :key "r"
  )
(transient-toggle-mode! highlight-changes-visible-mode ()
  "Highlight-changes"
  :key "x"
  )
(transient-toggle-mode! reveal-mode ()
  "Reveal Invisible"
  :key "I"
  )
(transient-toggle-mode! auto-highlight-symbol-mode ()
  "Auto Highlight Symbol"
  :key "H"
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
(transient-call!  quickscope ()
  "QuickScope"
  :key "s"
  :desc (transient-mode-fmt "QuickScope" evil-quickscope-always-mode "l")
  (evil-quickscope-always-mode 'toggle)
  (evil-quickscope-mode (if evil-quickscope-always-mode -1 1))
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
     (transient-macro-toggle-evil-goggles-mode)
     (transient-macro-toggle-hl-line-mode)
     (transient-macro-toggle-invisible)
     (transient-macro-toggle-highlight-changes-visible-mode)
     (transient-macro-call-link-display)
     ]
    [
     (transient-macro-toggle-rainbow-mode)
     (transient-macro-toggle-global-prettify-symbols-mode)
     (transient-macro-call-quickscope)
     (transient-macro-toggle-highlight-parentheses-mode)
     (transient-macro-call-elide-head)
     ]
    [
     (transient-macro-toggle-auto-highlight-symbol-mode)
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
