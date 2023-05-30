;;; +advice.el -*- lexical-binding: t; -*-

(when (modulep! :util text-manipulation +fold-jump)
  (define-advice evil-beginning-of-visual-line (:after ()
                                              +jg-text-invisi-line-respect)
  " When theres a vimish fold, move to the start of it "
  (let* ((overlays (overlays-at (point)))
         (types    (-filter (lambda (x) (eq 'vimish-fold--folded
                                            (overlay-get x 'type)))
                            overlays))
         )
    (if types
        (progn (goto-char (1- (overlay-start (car types))))
               (evil-next-line)
               )
      )
    )
  )

  (define-advice evil-next-line (:after (&optional count)
                               +jg-text-invisi-line-respect)
  " when theres a fold, jump past it "
  (let* ((overlays (overlays-at (point)))
         (types    (-filter (lambda (x) (eq 'vimish-fold--folded
                                            (overlay-get x 'type)))
                            overlays))
         )
    (if (and types (< (overlay-start (car types)) (point)))
        (progn (goto-char (overlay-end (car types)))
               (forward-line)
               )
      )
    )
  )

  (define-advice evil-previous-line (:after (&optional count)
                                   +jg-text-invisi-line-respect
                                   )
  " When theres a fold, jump past it "
  (let* ((overlays (overlays-at (point)))
         (types    (-filter (lambda (x) (eq 'vimish-fold--folded
                                            (overlay-get x 'type)))
                            overlays))
         )
    (if types
        (goto-char (overlay-start (car types)))
      )
    )
  )

  (define-advice evil-backward-char (:after (&optional count crosslines noerror)
                                   +jg-text-invisi-line-respect
                                   )
  " When theres a fold, jump to its start "
  (let* ((overlays (overlays-at (point)))
         (types    (-filter (lambda (x) (eq 'vimish-fold--folded (overlay-get x 'type))) overlays))
         )
    (if types
        (goto-char (overlay-start (car types)))
      )
    )
  )
)
