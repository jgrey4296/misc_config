;;; evil-ex.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+evil:reverse-lines "editor/text-manipulation/autoload/evil-ex" nil t)
(evil-define-command +evil:reverse-lines (beg end)
  "Reverse lines between BEG and END."
  (interactive "<r>")
  (reverse-region beg end))
