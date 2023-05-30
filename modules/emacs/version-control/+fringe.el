;;; +fringe.el -*- lexical-binding: t; -*-


;; UI: make the fringe small enough that the diff bars aren't too domineering,
;;   while leaving enough room for other indicators.
(if (fboundp 'fringe-mode) (fringe-mode '8))
;; UI: the gutter looks less cramped with some space between it and  buffer.
(setq-default fringes-outside-margins t)

;; STYLE: Redefine fringe bitmaps to take up only half the horizontal space in
;;   the fringe. This way we avoid overbearingly large diff bars without
;;   having to shrink the fringe and sacrifice precious space for other fringe
;;   indicators (like flycheck or flyspell).
;; REVIEW: Extract these into a package with faces that themes can target.
(if (not (modulep! +diff-hl))
    (after! git-gutter-fringe
      (define-fringe-bitmap 'git-gutter-fr:added [224]
        nil nil '(center repeated))
      (define-fringe-bitmap 'git-gutter-fr:modified [224]
        nil nil '(center repeated))
      (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
        nil nil 'bottom))
  (defadvice! +vc-gutter-define-thin-bitmaps-a (&rest args)
    :override #'diff-hl-define-bitmaps
    (define-fringe-bitmap 'diff-hl-bmp-middle [224] nil nil '(center repeated))
    (define-fringe-bitmap 'diff-hl-bmp-delete [240 224 192 128] nil nil 'top))
  (defun +vc-gutter-type-face-fn (type _pos)
    (intern (format "diff-hl-%s" type)))
  (defun +vc-gutter-type-at-pos-fn (type _pos)
    (if (eq type 'delete)
        'diff-hl-bmp-delete
      'diff-hl-bmp-middle))
  (advice-add #'diff-hl-fringe-bmp-from-pos  :override #'+vc-gutter-type-at-pos-fn)
  (advice-add #'diff-hl-fringe-bmp-from-type :override #'+vc-gutter-type-at-pos-fn)
  (setq diff-hl-draw-borders nil)
  (add-hook! 'diff-hl-mode-hook
    (defun +vc-gutter-fix-diff-hl-faces-h ()
      (mapc (doom-rpartial #'set-face-background nil)
            '(diff-hl-insert
              diff-hl-delete
              diff-hl-change)))))

;; FIX: To minimize overlap between flycheck indicators and git-gutter/diff-hl
;;   indicators in the left fringe.
(after! flycheck
  ;; Let diff-hl have left fringe, flycheck can have right fringe
  (setq flycheck-indication-mode 'right-fringe)
  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))
