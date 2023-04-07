;;; +text-obj.el -*- lexical-binding: t; -*-

(evil-define-text-object +jg-text-visual-contract (count)
  " Contract the selection to just the point "
  :type inclusive
  (interactive)
  (list (point) (1+ (point)))
  )

(evil-define-text-object +jg-text-grow-selection-op (count)
  " Grow the selection on either side by count "
  :type inclusive
  :extend-selection t
  (interactive)
  (list (- evil-visual-beginning 1) (+ evil-visual-end 1))
  )

(evil-define-text-object +jg-text-line-textobj (count)
  " Line object of the current line "
  :type line
  (interactive)
  (list (line-beginning-position) (line-end-position))
  )

(evil-define-text-object +jg-text-whole-buffer-textobj (count)
  " Line object of the current line "
  :type inclusive
  (interactive)
  (list (point-min) (point-max))
  )

(evil-define-text-object +jg-text-blank-block (count &rest rest)
  :type inclusive
  :extend-selection t
  (interactive)
  (save-excursion
    (let (beg end)
      (goto-char evil-visual-beginning)
      (re-search-backward "[[:graph:]]" nil t)
      (forward-line 2)
      (setq beg (line-beginning-position))
      (re-search-forward "[[:graph:]]" nil t)
      (forward-line -0)
      (setq end (line-beginning-position))
      (list beg end)
      )
    )
  )
