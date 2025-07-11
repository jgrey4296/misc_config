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
                            ("\\btype\\b"      (0 '(:background "flycheck-error-list-id") t))
                            ("^\s+def\\b"      (0 '(:background "mediumpurple4"                        :foreground "black") t))
                            ("\\bself\\."      (0 '(:background "slategray"                            :foreground "black") t))
                            ("\\bcase\\b"      (0 '(:background "mediumpurple2"                        :foreground "black") t))
                            ("\\bwith\\b"      (0 '(:background "mediumseagreen"                       :foreground "black") t))
                            ("^\s+raise\\b"    (0 '(:background "mediumvioletred"                      :foreground "black") t))
                            ("^\s+return\\b"   (0 '(:background "mediumspringgreen"                    :foreground "black") t))
                            ("\\b\\(breakpoint\\|assert\\)\b" (0 '(:background "mediumvioletred"      :foreground "black") t))
                            )
                          )
  )
