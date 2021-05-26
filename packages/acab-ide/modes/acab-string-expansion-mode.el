;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; TODO a tracery-inspired string expansion helper for authoring grammars
(provide 'acab-string-expansion-mode)

(define-minor-mode acab-string-expansion-minor-mode
  " Mode for manipulating string possibility grammars "
  :lighter "acab-string-expansion"
                                        ; :global t
                                        ; :keymap nil

  )

(defun acab-string-expansion-minor-mode/turn-on ()
  (unless (minibufferp)
    (if (eq major-mode 'prog-mode)
        (acab-string-expansion-minor-mode 1))
    )
  )

(define-globalized-minor-mode global-acab-string-expansion-minor-mode acab-string-expansion-minor-mode/turn-on)
