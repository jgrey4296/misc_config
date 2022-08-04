;;; +advice.el -*- lexical-binding: t; -*-

(define-advice evil-buffer-new (:after (count file)
                                +jg-new-buffer-force-lisp)
  (with-current-buffer (window-buffer)
      (emacs-lisp-mode)
      )
  )
