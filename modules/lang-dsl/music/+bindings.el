;; sclang keybindings.el
;; loaded fifth

(map! :leader
      :prefix ("o s" . "Systems")
      :desc "Start Music System" "M" 'jg-music-layer/start-system
      )

(map! :after csound-mode
      :map csound-mode-map
      :localleader

      )

(map! :map sclang-mode-map
      :localleader
      )

(map! :map chuck-mode-map
      :localleader
      )

(map! :map tidal-mode-map
      :localleader
      )
