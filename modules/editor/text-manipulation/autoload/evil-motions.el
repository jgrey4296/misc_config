;;; util/text/+motions.el -*- lexical-binding: t; -*-
;; https://evil.readthedocs.io/en/latest/extension.html
;; Reminder:
;; Motion       : Moves cursor,
;; Text Objects : A Motion Selection
;; Operators    : Act on text moved over by motion

;; Motions


;;;###autoload (autoload '+jg-text-prev-close-paren-motion "editor/text-manipulation/autoload/evil-motions.el" nil t)
(evil-define-motion +jg-text-prev-close-paren-motion (count)
  "Go to [count] next unmatched ')'."
  :type inclusive
  (backward-char)
  (search-backward")" nil t)
  (forward-char)
  )

;;;###autoload (autoload '+jg-text-next-open-paren-motion "editor/text-manipulation/autoload/evil-motions.el" nil t)
(evil-define-motion +jg-text-next-open-paren-motion (count)
  "Go to [count] next unmatched ')'."
  :type inclusive
  (forward-char)
  (search-forward "(" nil t)
  (backward-char)
  )

;;;###autoload (autoload '+jg-text-next-empty-line-motion "editor/text-manipulation/autoload/evil-motions.el" nil t)
(evil-define-motion +jg-text-next-empty-line-motion (count)
  :type line
  (forward-line)
  (re-search-forward "^[[:space:]]*$" nil nil (or count 1))
  )

;;;###autoload (autoload '+jg-text-prev-empty-line-motion "editor/text-manipulation/autoload/evil-motions.el" nil t)
(evil-define-motion +jg-text-prev-empty-line-motion (count)
  :type line
  (forward-line -1)
  (re-search-backward "^[[:space:]]*$" nil nil (or  count 1))
  )

;;;###autoload (autoload '+jg-text-next-similar-string "editor/text-manipulation/autoload/evil-motions.el" nil t)
(evil-define-motion +jg-text-next-similar-string (count)
  (interactive)
  (let* ((bound (or current-prefix-arg jg-text-last-similarity-arg))
         (curr-sim (+ bound 1))
         (s2 (downcase (string-trim (car (s-split ":" (buffer-substring (line-beginning-position) (line-end-position)))))))
         s1)
    (while (and (< (point) (point-max))
                (> curr-sim bound))
      (forward-line)
      (setq jg-text-last-similarity-arg bound)
      (setq s1 s2)
      (setq s2 (downcase (string-trim (car (s-split ":" (buffer-substring (line-beginning-position) (line-end-position)))))))
      (setq curr-sim (org-babel-edit-distance s1 s2))
      )
    )
  )

;;;###autoload (autoload '+jg-text-force-column-motion "editor/text-manipulation/autoload/evil-motions.el" nil t)
(evil-define-motion +jg-text-force-column-motion (count)
  "Force Go to column COUNT on the current line.
Columns are counted from zero."
  :type inclusive
  (move-to-column (or count (read-number "Column: ")) t)
  )

;;;###autoload (autoload '+jg-text-column-motion "editor/text-manipulation/autoload/evil-motions.el" nil t)
(evil-define-motion +jg-text-column-motion (count)
  "Augment evil-goto-column to go to end of line if at beginning"
  (cond (count
         (move-to-column count))
        ((< (point) (line-end-position))
         (end-of-line))
        (t
         (move-to-column 0))
        )
  )
