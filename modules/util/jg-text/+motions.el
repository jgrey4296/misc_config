;;; util/text/+motions.el -*- lexical-binding: t; -*-
;; https://evil.readthedocs.io/en/latest/extension.html
;; Reminder:
;; Motion       : Moves cursor,
;; Text Objects : A Motion Selection
;; Operators    : Act on text moved over by motion

;; Motions
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
  :type line
  (forward-line)
  (re-search-forward "^[[:space:]]*$" nil nil (or count 1))
  )
(evil-define-motion +jg-text-prev-empty-line-motion (count)
  :type exclusive
  (forward-line -1)
  (re-search-backward "^[[:space:]]*$" nil nil (or  count 1))
  )

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

(evil-define-motion +jg-text-force-column-motion (count)
  "Force Go to column COUNT on the current line.
Columns are counted from zero."
  :type exclusive
  (move-to-column (or count 0) t))


;; Text Objects
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
(evil-define-text-object +jg-text-whole-buffer-textobj (count)
  " Line object of the current line "
  :type inclusive
  (interactive)
  (list (point-min) (point-max))
  )

;; Operators
(evil-define-operator +jg-text-split-on-char-op (beg end)
  ;; TODO
  :move-point t
  (message "Count: %s" count)
  (let ((last-char (downcase (char-after (+ (point) count))))
        curr-char)
    (while (< (point) end)
      (setq curr-char (downcase (char-after (+ (point) count))))
      (if (not (eq last-char curr-char))
          (progn
            (setq last-char curr-char)
            (goto-char (line-beginning-position))
            (insert "\n")
            )
        )
      (forward-line)
      )
    )
  )
(evil-define-operator +jg-text-remove-leading-whitespace-op (beg end count)
  :move-point t
  (while (< (point) end)
    (evil-first-non-blank)
    (delete-region (line-beginning-position) (point))
    (forward-line)
    )
  )
(evil-define-operator +jg-text-uniquify-op (beg end count)
  :move-point t
  (evil-first-non-blank)
  (let ((R-mark (set-marker (make-marker) end))
        (current (+jg-get-line))
        (kill-whole-line t))
    (forward-line 1)
    (while (<= (point) R-mark)
      ;; compare
      (if (s-equals? current (+jg-get-line))
          (kill-line)
        (progn (setq current (+jg-get-line))
               (forward-line 1))
        )
      )
    )
  )
(evil-define-operator +jg-text-escalate-replace-op (beg end count)
  " Replace a regex in the region,
with either a numeric or alphabetical escalation "
  :move-point t
  (let* ((end-mark (set-marker (make-marker) end))
         (reg  (read-string "Regexp: " "^.*?\\( \\)"))
         (base (read-string "Replacement Base: "))
         (type (car (read-multiple-choice "Increment Digit or Char? "
                                          '((?d "Digit")
                                            (?c "Char")))))
         (current (if (eq ?d type) 0 ?a))
         )
    (goto-char beg)
    (while (re-search-forward reg end-mark t)
      (replace-match (format "%s_%s" base (if (eq ?c type) (char-to-string current) current))
                     t nil nil 1)
      (incf current)
      (if (and (eq type ?c) (> current ?z)) (setq current ?a))
      )
    )
  )
(evil-define-operator +jg-text-title-case-op (beg end)
  :move-point t
  (while (< (point) end)
    (capitalize-word 1)
    )
  )
(evil-define-operator +jg-text-simple-grep-op (beg end count)
  :move-point t
  (interactive)
  (let ((reg (read-string "Match Regexp: ")))
    (with-temp-buffer-window "*Text-Results*" 'display-buffer-pop-up-window nil
      (goto-char (point-min))
      (while (re-search-forward reg end t)
        (princ (format "%s : %s\n" (line-number-at-pos) (buffer-substring (line-beginning-position) (line-end-position))))
        )
      )
    )
  (let ((inhibit-read-only t))
    (with-current-buffer "*Text-Results*"
      (align-regexp (point-min) (point-max) "\\(\s-*\\):")
      )
    )
  )

(evil-define-operator +jg-text-line-on-char-op (beg end count)
  ;; TODO

  )

(evil-define-operator +jg-text-goto-random-line-op (beg end)
  :motion +evil:whole-buffer-txtobj
  :repeat t
  :move-point t
  (let* ((min-line (line-number-at-pos beg))
         (max-line (- (line-number-at-pos end) min-line))
         )
    (forward-line (random max-line))
    (evil-first-non-blank-of-visual-line)
    )
  )
