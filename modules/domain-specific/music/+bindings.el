;; sclang keybindings.el
;; loaded fifth

(map! :after jg-leader-bindings-loaded
      :leader
      :prefix ("o s" . "Systems")
      :desc "Start Music System" "M" 'jg-music-layer/start-system
      )

(map! :after csound-mode
      :map csound-mode-map
      :localleader
      :desc "Docs: Csound Manual" "1" (cmd! (+jg-browse-url "https://csound.com/docs/manual/PartOverview.html"))
      :desc "Docs: Csound Tutorial" "2" (cmd! (+jg-browse-url "http://www.csounds.com/toots/index.html"))

      )

(map! :map sclang-mode-map
      :localleader
      :desc "Docs: Supercollider" "1" (cmd! (+jg-browse-url "http://doc.sccode.org/"))
      )

(map! :map chuck-mode-map
      :localleader
      :desc "Docs: ChucK manual" "1" (cmd! (+jg-browse-url "https://chuck.cs.princeton.edu/doc/"))
      )

(map! :map tidal-mode-map
      :localleader
      :desc "Docs: Tidal" "1" (cmd! (+jg-browse-url "https://tidalcycles.org/docs/"))
      )
