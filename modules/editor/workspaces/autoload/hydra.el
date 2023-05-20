;;; +hydra.el -*- lexical-binding: t; -*-
;;
;; TODO hydra for control of workspaces, windows, window-ring settings

;;;###autoload
(after! hydra
  (defhydra hydra-workspace ()
    (format "%s\n" (+jg-hydra-format-columns
                    '("|Projects"
                      )
                    '("|Workspaces"

                      )
                    '("|Windows"
                      "Auto-_b_alance"
                      "horizontal Shrink"
                      "vertical Shrink"
                      )
                    '(blank
                      "%-10(+jg-hydra-doc evil-auto-balance-windows)"
                      )
                    '("|Window-Ring"
                      new
                      convert
                      deconvert
                      "_K_ clear"
                      reset
                      print
                      Edit
                      )
                    '("%-10(persp-parameter 'window-ring)"
                      loop
                      expand
                      add
                      Remove
                      "c_h_oose"
                      )
                    '(blank
                      "%-4(persp-parameter 'window-ring-loop)"
                      )
                    ))
    ;; Workspaces

    ;; Windows
    ("h" shrink-window-horizontally nil :exit nil)
    ("v" shrink-window nil :exit nil)
    ("b" (setq evil-auto-balance-windows (not evil-auto-balance-windows))  nil :exit nil)

    ;; Window Ring
    ("p" window-ring-print-order)
    ("E" window-ring-edit-order)
    ("l" window-ring-toggle-loop)
    ("n" window-ring-new)
    ("c" window-ring-convert)
    ("d" window-ring-deconvert)
    ("e" window-ring-shrink-sides)
    ("r" window-ring-reset-columns)
    ("K" window-ring-clear-ring)
    ("a" window-ring-add-current-buffer)
    ("R" window-ring-remove-buffer)
    ("h" nil)
    )
  )
