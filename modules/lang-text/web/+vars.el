;;; lang-text/web/+vars.el -*- lexical-binding: t; -*-

(defvar +web-continue-block-comments t
  "If non-nil, newlines in block comments are continued with a leading *.

This also indirectly means the asterisks in the opening /* and closing */ will
be aligned.

If set to `nil', disable all the above behaviors.")

;;-- css
(setq helm-css-scss-split-direction #'split-window-vertically
      helm-css-scss-split-with-multiple-windows t
      )

;;-- end css

;;-- web mode
(setq web-mode-enable-auto-expanding t
      web-mode-enable-auto-closing   t
      web-mode-enable-css-colorization nil
      web-mode-enable-block-face       nil
      web-mode-enable-auto-indentation nil
      web-mode-enable-auto-quoting     nil
      web-mode-enable-block-face       nil
      web-mode-enable-control-block-indentation t
      web-mode-enable-engine-detection t
      web-mode-enable-html-entities-fontification t
      web-mode-enable-auto-quoting nil
      web-mode-enable-auto-pairing t

      web-mode-markup-indent-offset 4
      web-mode-css-indent-offset    4
      web-mode-code-indent-offset   4
      web-mode-style-padding        1
      web-mode-script-padding       1
      web-mode-comment-style        1
      web-mode-auto-close-style     1
      )

(setq web-mode-indentation-params '(
                                    ("lineup-args" . t)
                                    ("lineup-calls" . t)
                                    ("lineup-concats" . t)
                                    ("lineup-quotes" . t)
                                    ("lineup-ternary" . t)
                                    ("case-extra-offset" . t)
                                    )
)

(setq web-mode-engines-alist '(
                               ("phoenix" . "\\.[lh]eex\\'")
                               ("elixir" . "\\.eex\\'")
                               ("php"    . "\\\\.phtml\\\\\\='")
                               ("blade"  . "\\\\.blade\\\\.")
                               )
      )

(setq web-mode-content-types-alist '(
                                     ;; ("json" . "/some/path/.*\\.api\\\\='")
                                     ;; ("jsx"  . "/some/react/path/.*\\.js[x]?\\\\='")
                                     ("jinja" . "_templates/*\\.html'")
                                     )
      )
;;-- end web mode

;;-- projectile
(after! projectile
  (pushnew! projectile-other-file-alist
            '("css"  "scss" "sass" "less" "styl")
            '("scss" "css")
            '("sass" "css")
            '("less" "css")
            '("styl" "css")))

;;-- end projectile

;;-- specs
(spec-handling-add! fold
                    '(web
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
                    '(css
                      :modes (css-mode)
                      :priority 25
                      :triggers  (:open-all   nil
                                  :close-all  nil
                                  :toggle     +css/toggle-inline-or-block
                                  :open       nil
                                  :open-rec   nil
                                  :close      nil
                                  )
                      )
                    )
(spec-handling-add! company
                    `(alchemist-mode (:mode alchemist-company))
                    `(pug-mode       (:mode company-web-jade))
                    `(web-mode       (:favour company-css)      (:mode company-web-html))
                    `(slim-mode      (:mode company-web-slim))
                    `(sass-mode      (:mode company-css))
                    )
(spec-handling-add! tree-sit-lang
                    '(css-mode        . css)
                    '(html-mode       . html)
                    '(mhtml-mode      . html)
                    '(javascript-mode . javascript)
                    '(js-mode         . javascript)
                    '(js2-mode        . javascript)
                    '(js3-mode        . javascript)
                    '(typescript-mode . typescript)
                    )
(spec-handling-add! auto-modes
                    '(web
                      ("\\.[px]?html?\\'"                  . web-mode)
                      ("\\.erb\\'"                         . web-mode)
                      ("\\.[lh]?eex\\'"                    . web-mode)
                      ("\\.jsp\\'"                         . web-mode)
                      ("\\.as[cp]x\\'"                     . web-mode)
                      ("\\.ejs\\'"                         . web-mode)
                      ("\\.hbs\\'"                         . web-mode)
                      ("\\.mustache\\'"                    . web-mode)
                      ("\\.svelte\\'"                      . web-mode)
                      ("\\.twig\\'"                        . web-mode)
                      ("\\.jinja2?\\'"                     . web-mode)
                      ("\\.eco\\'"                         . web-mode)
                      ("wp-content/themes/.+/.+\\.php\\'"  . web-mode)
                      ("templates/.+\\.php\\'"             . web-mode)
                      ("\\.vue\\'"                         . web-mode)
                      ("\\.scss\\'"                        . scss-mode)
                      ("\\.css\\'"                         . css-mode)
                      ("\\.js"                             . javascript-mode)
                      ("\\.ts"                             . javascript-mode)
                      )
                    )
(spec-handling-add! docsets
                      '(web-mode
                        "HTML" "CSS" "Twig" "WordPress"
                        )
                      )
(spec-handling-add! babel
                    '(web
                      (:name css        :lib ob-css)
                      (:name js         :lib ob-js)
                      (:name sass       :lib ob-sass)
                      )
                    )
(spec-handling-add! org-src
                    '(web
                      ("html" . web)
                      ("css" . css)
                      ("sass" . sass)
                      ("js" . js)
                      )
                    )
(spec-handling-add! docsets
                    '((css-mode scss-mode sass-mode)
                      "CSS" "HTML" "Bourbon" "Compass"
                      "Sass"
                      )
                    )

(spec-handling-add! file-templates
                    '(javascript
                      ("\\.js\\'" :trigger "__"   :mode javascript-mode   :priority 100)
                      )
                    )
;;-- end specs
