(message "Loading modules/editor/jg-personal/+bindings.el")

(map! :mode vlf-mode
      "] A" 'vlf-next-batch-from-point
      "] a" 'vlf-next-batch
      "[ a" 'vlf-prev-batch
      "SPC a U v " 'vlf-set-batch-size
      )

(map! :leader
      :prefix "t"
      :desc "Semantic" "S" #'semantic-mode
      )

;; todo add semantic etc
