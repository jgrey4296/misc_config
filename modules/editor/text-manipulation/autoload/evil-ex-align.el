;;; align-highlight.el -*- lexical-binding: t; -*-

(defvar jg-text-manip-auto-align-groups '("/:/" "/=/"))

(defvar jg-text-manip-auto-align-visual nil)

(evil-ex-define-argument-type jg-align-regexp
  " Like the highlighter for evil-ex-substitute, but for alignment "
  :runner (lambda (flag &optional arg)
            (+jg-text-manipulation-highlight-handler "\\(\\s-*\\)" flag arg))
  )

(evil-define-interactive-code "<al/>"
  "Ex Align argument"
  :ex-arg jg-align-regexp
  (when evil-called-from-ex-p
    (+jg-text-manipulation-get-pattern-info evil-ex-argument t "\\(\\s-*\\)")
    )
  )

;;;###autoload (autoload '+jg-text-manipulation-ex-align-highlight "editor/text-manipulation/autoload/evil-ex-align" nil t)
(evil-define-command +jg-text-manipulation-ex-align-highlight (beg end &optional pattern case wholeline)
  "Ex interface to `align-regexp'.

PATTERN is a vim-style regexp. FLAGS is an optional string of characters.
Supports the following flags:

g   Repeat alignment on all matches in each line"
  (interactive "<r><al/>")
  (when (car-safe pattern)
    (align-regexp beg end (car pattern) 1 1 nil)
    )
  )

;;;###autoload (autoload '+jg-text-manipulation-ex-expand-align-highlight "editor/text-manipulation/autoload/evil-ex-align" nil t)
(evil-define-command +jg-text-manipulation-ex-expand-align-highlight (&optional pattern case wholeline blanks minimal)
  "Ex interface to `align-regexp', but auto-expands the selection for lines that match the regexp

PATTERN is a vim-style regexp. FLAGS is an optional string of characters.
Supports the following flags:

- g   Repeat alignment on all matches in each line

Pass 'blanks to expand the selection past empty lines
Pass 'minimal to compress whitespace first before aligning
"

  (interactive "<al/>")
  (let ((start (point))
        (beg (line-beginning-position))
        (end (line-end-position 2))
        (regexp (when (car-safe pattern) (rx line-start (| line-end (: (+? any) (regexp (car pattern)))))))
        (repreg (format "^\\(.+?\\)\\([[:blank:]]+%c\\)" (string-to-char (string-reverse (car pattern)))))
        (replace (format "  %c" (string-to-char (string-reverse (car pattern)))))
        )
    (when regexp
      (save-excursion
        (evil-beginning-of-line)
        ;; Expand up
        (condition-case err
            (while (and (looking-at regexp)
                        (not (or (bobp)
                                 (and (not blanks)
                                      (eq (line-beginning-position) (line-end-position)))
                                 )))
              (line-move -1)
              (move-beginning-of-line 1))
          ;; Handlers:
          (:success (setq beg (line-beginning-position)))
          (t (setq beg (point-min)))
          )

        (goto-char end)
        (move-beginning-of-line 1)
        ;; Expand Down
        (condition-case err
            (while (and (looking-at regexp)
                        (not (or (eobp)
                                 (and (not blanks)
                                      (eq (line-beginning-position) (line-end-position))
                                      ))
                             )
                        )
              (line-move 1)
              (move-beginning-of-line 1))
          ;; Handlers:
          (:success (setq end (line-end-position)))
          (t (setq end (point-max)))
          )
        )
      )
    (save-excursion
      (goto-char beg)
      (while (and minimal (re-search-forward repreg end t))
        (replace-match replace nil nil nil 2)
        )
      ;; Run align
      (align-regexp beg end (car pattern) 1 1 nil)
      )
    )
  )

;;;###autoload
(defun +jg-text-manipulation-ex-auto-align ()
  " Runs sucessive align ops  "
  (interactive)
  (cl-loop for group in jg-text-manip-auto-align-groups
           for align-re = (+jg-text-manipulation-get-pattern-info group t "\\(\\s-*\\)")
           do
           (save-excursion
             (apply #'+jg-text-manipulation-ex-expand-align-highlight (list (car align-re) nil nil t t))
             )
           )
  )
