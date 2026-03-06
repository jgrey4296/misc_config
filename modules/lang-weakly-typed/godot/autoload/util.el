;;; util.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;;###autoload
(defun +jg-gd-swap-ts ()
  (interactive)
  (pcase major-mode
    ('gdscript-mode
     (evil-open-folds)
     (gdscript-ts-mode)
     )
    ('gdscript-ts-mode
     (evil-open-folds)
     (gdscript-mode)
     )
    (_ (user-error "unknown gdscript mode"))
    )
  )


;;;###autoload
(defun jg-gd-font-lock-mod-h()
  (font-lock-add-keywords nil
                          '(
                            ("^\s+return\\b"   (0 '(:background "mediumspringgreen" :foreground "black") t))
                            )
                          )
  )


;;; util.el ends here
