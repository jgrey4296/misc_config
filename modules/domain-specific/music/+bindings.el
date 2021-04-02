;; sclang keybindings.el
;; loaded fifth
(message "Loading modules/domain-specific/music/+bindings.el")

(map! :leader
      :prefix ("a s" . "Systems")
      :desc "Start Music System" "M" 'jg-music-layer/start-system
      )
