(defconst overlay_control-packages
  '(
    helm
    (overlay-ctrl-minor-mode :location local)
    (font-lock+ :location (recipe :fetcher git :url "https://github.com/emacsmirror/font-lock-plus"))
    )
  )

