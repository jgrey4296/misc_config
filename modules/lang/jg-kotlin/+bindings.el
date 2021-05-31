;;; lang/jg-kotlin/+bindings.el -*- lexical-binding: t; -*-

(map! :map kotlin-mode-map
      :localleader
      :desc "Jump to Docs" "1" (cmd! (+jg-browse-url "https://developer.android.com/docs"))
)
