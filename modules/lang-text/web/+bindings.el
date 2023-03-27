;;; lang-text/web/+bindings.el -*- lexical-binding: t; -*-

(map! :map scss-mode-map
      :localleader
      "b" #'+css/scss-build
      )
(map! :map (css-mode-map scss-mode-map less-css-mode-map)
      "rb" #'+css/toggle-inline-or-block
      )

(map! :map (css-mode-map scss-mode-map less-css-mode-map)
          :localleader ";" #'counsel-css)

(map! :map (css-mode-map scss-mode-map less-css-mode-map)
        :localleader ";" #'helm-css-scss)
(map! :map sass-mode-map
      :localleader
      "b" #'+css/sass-build)
