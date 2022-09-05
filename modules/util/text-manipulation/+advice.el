;;; +advice.el -*- lexical-binding: t; -*-

(define-advice evil-beginning-of-visual-line (:after ()
                                              +jg-text-invisi-line-respect)
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
  (let* ((overlays (overlays-at (point)))
         (types    (-filter (lambda (x) (eq 'vimish-fold--folded
                                            (overlay-get x 'type)))
                            overlays))
         )
    (if types
        (progn (goto-char (overlay-end (car types)))
               (evil-backward-char)
               )
      )
    )
  )

(define-advice evil-previous-line (:after (&optional count)
                                   +jg-text-invisi-line-respect
                                   )
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
  (let* ((overlays (overlays-at (point)))
         (types    (-filter (lambda (x) (eq 'vimish-fold--folded (overlay-get x 'type))) overlays))
         )
    (if types
        (goto-char (overlay-start (car types)))
      )
    )
  )

(define-advice evil-join (:after (beg end)
                          +jg-text-join-line-bol)
  (beginning-of-line)
  )
