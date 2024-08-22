;;; transient-visuals.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;; Visual
(progn
  (transient-make-mode-toggle! global-prettify-symbols-mode   "Pretty Symbols" "p")
  (transient-make-mode-toggle! hl-line-mode                   "Highlight-line"    "h")
  (transient-make-mode-toggle! evil-goggles-mode              "Evil-goggles"      "g")
  (transient-make-mode-toggle! highlight-parentheses-mode     "Higlight-wrappers" "w")
  (transient-make-mode-toggle! rainbow-mode                   "Rainbow Colours"   "r")
  (transient-make-mode-toggle! highlight-changes-visible-mode "Highlight-changes" "x")
  (transient-make-mode-toggle! reveal-mode                    "Reveal Invisible" "I")
  (transient-make-mode-toggle! auto-highlight-symbol-mode     "Auto Highlight Symbol" "H")

  ;;
  (transient-make-call! elide-head "e"
                        (transient-title-mode-formatter "Elide Head" (if (boundp 'elide-head-overlay) elide-head-overlay "-1") "e")
                        (if (boundp 'elide-head-overlay) (elide-head-show) (elide-head))
                        )

  (transient-make-call! link-display "l"
                        (transient-title-mode-formatter "LinkDisplay" org-link-descriptive "l")
                        (org-toggle-link-display))
  (transient-make-call!  quickscope "s"
                         (transient-title-mode-formatter "Quickscope" evil-quickscope-always-mode "s")
                         (evil-quickscope-always-mode 'toggle)
                         (evil-quickscope-mode (if evil-quickscope-always-mode -1 1))
                         )

  (transient-make-var-toggle!   invisible line-move-ignore-invisible "Invisible Spec" "i")
  )

;;;###autoload (autoload #'jg-toggle-visuals-transient "config/ui/autoload/transient-visuals" nil t)
(transient-make-subgroup! jg-toggle-visuals-transient "v"
                          "For controlling ui visual settings"
                          :desc "|| Visuals    ||"
                          [:description "|| Visuals    ||"
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
                           ]
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
