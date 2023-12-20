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
  (spec-handling-add! docsets
                      '((css-mode scss-mode sass-mode)
                        "CSS" "HTML" "Bourbon" "Compass"
                        "Sass"
                        )
                      )
  )

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
                      ("\\.scss\\'" . scss-mode)
                      ("\\.css\\'" . css-mode)
                      )
                    )

  (spec-handling-add! docsets
                      '(web-mode
                        "HTML" "CSS" "Twig" "WordPress"
                        )
                      )
;;-- end specs

(spec-handling-add! lookup-regular
                    '((css-mode less-css-mode scss-mode sass-mode)
                     ("CSS Reference" . "https://developer.mozilla.org/en-US/docs/Web/CSS")
                     ("LESS Reference" . "https://lesscss.org/features/")
                     ("Cheatsheet" . "https://htmlcheatsheet.com/css/")
                     ("Css templates" . "https://markdowncss.github.io/")
                     )
                    '(web-mode
                     ("HTML Reference" . "https://developer.mozilla.org/en-US/docs/Web/HTML")
                     ("Cheatsheet" . "https://htmlcheatsheet.com/")
                     )
                    )
