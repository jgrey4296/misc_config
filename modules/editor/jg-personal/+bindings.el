
(defun +jg-personal-binding-hook ()
  (message "Setting up personal bindings: %s" (current-time-string))
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
)
;; todo add semantic etc
