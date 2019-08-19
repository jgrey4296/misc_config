(setq jg_layer/char-inserting-candidates-pairs (jg_layer/char-inserting-get-candidates)
      jg_layer/char-inserting-helm `((name . "Helm Insert Char")
                                     (action . (lambda (cand)
                                                 (mapcar (lambda (x) (insert-char (car x))) (helm-marked-candidates))))
                                     )
      )

