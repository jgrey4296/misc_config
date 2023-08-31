;;; +text-obj.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+jg-text-visual-contract "editor/text-manipulation/autoload/evil-text-obj" nil t)
(evil-define-text-object +jg-text-visual-contract (&rest args)
  " Contract the selection to just the point "
  :type inclusive
  (list (point) (1+ (point)))
  )

;;;###autoload (autoload '+jg-text-grow-selection-op "editor/text-manipulation/autoload/evil-text-obj" nil t)
(evil-define-text-object +jg-text-grow-selection-op (count &rest args)
  " Grow the selection on either side by count "
  :type inclusive
  :extend-selection t
  (list (- evil-visual-beginning (or count 1)) (+ evil-visual-end (or count 1)))
  )

;;;###autoload (autoload '+jg-text-line-textobj "editor/text-manipulation/autoload/evil-text-obj" nil t)
(evil-define-text-object +jg-text-line-textobj (count &rest args)
  " Line object of the current line "
  :type line
  (list (line-beginning-position) (line-end-position))
  )

;;;###autoload (autoload '+jg-text-whole-buffer-textobj "editor/text-manipulation/autoload/evil-text-obj" nil t)
(evil-define-text-object +jg-text-whole-buffer-textobj (count &rest args)
  " Line object of the current line "
  :type inclusive
  (list (point-min) (point-max))
  )

;;;###autoload (autoload '+jg-text-blank-block "editor/text-manipulation/autoload/evil-text-obj" nil t)
(evil-define-text-object +jg-text-blank-block (count &rest args)
  :type inclusive
  :extend-selection t
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

;;;###autoload (autoload '+jg-text-spaces "editor/text-manipulation/autoload/evil-text-obj" nil t)
(evil-define-text-object +jg-text-spaces (count &rest args)
  "select spaces on the same line"
  :type inclusive
  :extend-selection t
  (let (beg end)
    (save-excursion
      (setq beg (+ 2 (re-search-backward (rx (not blank)) (save-excursion (beginning-of-line)) t)))
      (setq end (1- (re-search-forward (rx blank (| graph eol)) (save-excursion (end-of-line)))))
      )
    (list beg end)
    )
  )
