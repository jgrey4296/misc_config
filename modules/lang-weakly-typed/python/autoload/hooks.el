;;; hooks.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +jg-python-outline-regexp-override-hook ()
  (setq-local outline-regexp jg-python-outline-regexp
              outline-heading-end-regexp jg-python-outline-end-regexp
              outline-level #'+jg-python-outline-level
              )
  )


;;;###autoload
(defun jg-python-font-lock-mod-h ()
  (font-lock-add-keywords nil
                          '(
                            ("^\s+return "  (0 '(:background "mediumpurple4") t))
                            ("^\s+def "     (0 '(:background "mediumpurple4") t))
                            ("breakpoint()" (0 '(:background "Mediumvioletred") t))
                            ("^type"        (0 '(:background "flycheck-error-list-id") t))
                            )
                          )
  )
