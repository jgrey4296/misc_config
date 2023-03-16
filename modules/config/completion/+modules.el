;;; +modules.el -*- lexical-binding: t; -*-


;; Ivy for helping adjust module activation
;;

(defun +jg-completion-modules-ivy ()
  (interactive)

  (ivy-read "Modules: "
            '()
            )
  )
