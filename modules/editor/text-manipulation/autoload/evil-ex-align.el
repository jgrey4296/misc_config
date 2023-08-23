;;; align-highlight.el -*- lexical-binding: t; -*-

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
(evil-define-command +jg-text-manipulation-ex-expand-align-highlight (&optional pattern case wholeline)
  "Ex interface to `align-regexp', but auto-expands the selection for lines that match the regexp

PATTERN is a vim-style regexp. FLAGS is an optional string of characters.
Supports the following flags:

g   Repeat alignment on all matches in each line"
  (interactive "<al/>")
  (let ((start (point))
        (beg (line-beginning-position))
        (end (line-end-position 1))
        (regexp (when (car-safe pattern) (format "\\(^$\\)\\|\\(^.*?\\(%s\\)\\)" (car-safe pattern))))
        )
    (when regexp
      (save-excursion
        (evil-beginning-of-line)
        ;; Expand up
        (while (and (looking-at regexp) (not (bobp)))
          (unless (looking-at "^$") (setq beg (line-beginning-position)))
          (evil-previous-line)
          (evil-beginning-of-line))

        (goto-char end)
        (goto-char (line-beginning-position 2))

        ;; Expand Down
        (while (and (looking-at regexp) (not (eobp)))
          (unless (looking-at "^$") (setq end (line-end-position)))
          (evil-next-line)
          (evil-beginning-of-line))

        ;; Run align
        (align-regexp beg end (car pattern) 1 1 nil)
        )
      )
    )
  )
