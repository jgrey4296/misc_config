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

(advice-remove 'evil-beginning-of-visual-line 'evil-beginning-of-visual-line@+jg-text-invisi-line-respect)
(advice-remove 'evil-next-line 'evil-next-line@+jg-text-invisi-line-respect)
(advice-remove 'evil-previous-line 'evil-previous-line@+jg-text-invisi-line-respect)
(advice-remove 'evil-backward-char 'evil-backward-char@+jg-text-invisi-line-respect)
