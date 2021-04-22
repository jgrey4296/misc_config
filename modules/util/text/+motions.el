;;; util/text/+motions.el -*- lexical-binding: t; -*-
;; https://evil.readthedocs.io/en/latest/extension.html
;; Reminder:
;; Motion       : Moves cursor,
;; Text Objects : A Motion Selection
;; Operators    : Act on text moved over by motion

(evil-define-text-object +jg-text-line-object (count)
  "For selecting the entire line"
  :extend-selection t
  (list (line-beginning-position) (line-end-position))
  )

(evil-define-motion +jg-text-prev-close-paren-motion (count)
  "Go to [count] next unmatched ')'."
  :type exclusive
  (backward-char)
  (search-backward")" nil t)
  (forward-char))
(evil-define-motion +jg-text-next-open-paren-motion (count)
  "Go to [count] next unmatched ')'."
  :type exclusive
  (forward-char)
  (search-forward "(" nil t)
  (backward-char))

(evil-define-motion +jg-text-next-empty-line-motion (count)
  :type exclusive
  (forward-line)
  (re-search-forward "^[[:space:]]*$" nil nil count)

  )
(evil-define-motion +jg-text-prev-empty-line-motion (count)
  :type exclusive
  (forward-line -1)
  (re-search-backward "^[[:space:]]*$" nil nil count)
  )

(evil-define-text-object +jg-text-grow-selection-op (count)
  " Grow the selection on either side by count "
  :type exclusive
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

(evil-define-operator +jg-text-split-on-char (beg end count)
  ;; TODO

  )
(evil-define-operator +jg-text-remove-leading-whitespace-op (beg end count)
  ;; TODO
  )
(evil-define-operator +jg-text-uniquify-op (beg end count)
  ;; TODO
  )
(evil-define-operator +jg-text-line-on-char-op (beg end count)
  ;; TODO

  )
(evil-define-operator +jg-text-strip-spaces-op (beg end count)
 ;; TODO
  )
