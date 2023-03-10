;;; editor/overlay_control/+vars.el -*- lexical-binding: t; -*-
(defcustom overlay_control/helm-candidates
  '(("Red Foreground" . (font-lock-ignore t face (:foreground "red")))
    ("Green Background" . (font-lock-ignore t face (:background "green")))
    ("Full Hide" . (invisible t))
    ("Ellipsis Hide" . :display)
    )
  "Suggestions for overlays")

(defcustom overlay_control/hide-text "..." "The Text to substitute hidden entries with")

(defvar overlay_control/current-overlay-type (cdr (car overlay_control/helm-candidates)))
