;;; align-highlight.el -*- lexical-binding: t; -*-

(evil-define-interactive-code "<al/>"
  "Ex Align argument"
  :ex-arg align
  (when evil-called-from-ex-p
    (+jg-text-manipulation-get-pattern-info evil-ex-argument t "\\(\\s-*\\)")
    )
  )

(evil-ex-define-argument-type align
  " Like the highlighter for evil-ex-substitute, but for alignment "
  :runner (lambda (flag &optional arg)
            (+jg-text-manipulation-highlight-handler "\\(\\s-*\\)" flag arg))
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
