;;; +defines.el -*- lexical-binding: t; no-byte-compile: t; -*-


(defvar jg-global-map (make-keymap "jg-global-map"))

(defvar jgb-halting-keymap (list 'keymap (make-char-table 'halt #'ignore)))

(defvar jgb-ctl-x-map (make-sparse-keymap "jg-ctl-x-map"))


;; root / { text nontext } maps

(def-keymap-subtypes! jgb-jump text nontext)
(def-keymap-subtypes! jgb-change text nontext)
(def-keymap-subtypes! jgb-vision text nontext)
(def-keymap-subtypes! jgb-motion text nontext)

(def-named-keymap! jgb-help-map)
(def-named-keymap! jgb-info-map :sparse t)

;;; +defines.el ends here
