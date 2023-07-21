;;; debugging.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-bindings-name-all-maps ()
  (interactive)
  (let ((maps (current-active-maps))
        (mapcount 0)
        all-maps
        )
    (cl-do-all-symbols (sym)
      (when (and (keymapp (ffap-symbol-value sym))
                 (sequencep (ffap-symbol-value sym))
                 )
        (cl-incf mapcount)
        (push (symbol-name sym) all-maps)
        (set sym (append (symbol-value sym) `((:name ,sym))))
        )
      )

    (message "Map Names: %s" all-maps)
    (message "There are %s active maps\nThere are %s maps total" (length maps) mapcount)
    nil
    )
  )
