;;; ui/hydra/config.el -*- lexical-binding: t; -*-

(defadvice! +hydra--inhibit-window-switch-hooks-a (fn)
  :around #'lv-window
  (let (doom-switch-window-hook)
    (funcall fn)))
