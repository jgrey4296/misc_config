;;; lang-text/web/+vars.el -*- lexical-binding: t; -*-

(setq helm-css-scss-split-direction #'split-window-vertically
      helm-css-scss-split-with-multiple-windows t
      web-mode-enable-auto-expanding t
      web-mode-enable-auto-closing t
      )

(defvar +web-continue-block-comments t
  "If non-nil, newlines in block comments are continued with a leading *.

This also indirectly means the asterisks in the opening /* and closing */ will
be aligned.

If set to `nil', disable all the above behaviors.")

(after! projectile
  (pushnew! projectile-other-file-alist
            '("css"  "scss" "sass" "less" "styl")
            '("scss" "css")
            '("sass" "css")
            '("less" "css")
            '("styl" "css")))

(after! (:any css-mode sass-mode)
  (set-docsets! '(css-mode scss-mode sass-mode)
    "CSS" "HTML" "Bourbon" "Compass"
    ["Sass" (memq major-mode '(scss-mode sass-mode))]))

;;-- fold spec
(spec-handling-add! fold nil
                    ('web
                     :modes (web-mode)
                     :priority 25
                     :triggers (:open-all   nil
                                :close-all  nil
                                :toggle     web-mode-fold-or-unfold
                                :open       nil
                                :open-rec   nil
                                :close      nil
                                )
                     )
                    )
;;-- end fold spec
