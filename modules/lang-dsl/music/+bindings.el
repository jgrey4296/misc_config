;; sclang keybindings.el
;; loaded fifth

(defvar jg-sclang-mode-map (make-sparse-keymap))

(map! :leader
      :prefix ("o s" . "Systems")
      :desc "Start Music System" "M" 'jg-music-layer/start-system
      )

(map! :after csound-mode
      :map csound-mode-map
      :localleader

      )

(map! :map jg-sclang-mode-map
      :v "RET" #'sclang-eval-region
      :n "RET" #'+jg-sclang-save-and-run-line
      :localleader
      "e" #'+jg-sclang-clear-and-run-buffer
      "d" #'sclang-dump-interface
      "s" #'sclang-start
      "q" #'sclang-stop
      )

(map! :map chuck-mode-map
      :localleader
      )

(map! :map tidal-mode-map
      "C-c C-c" nil
      :localleader
      )


(setq sclang-mode-map jg-sclang-mode-map)
