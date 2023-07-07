;;; vars.el -*- lexical-binding: t; -*-

;;;###autoload
(defconst +latex-indent-item-continuation-offset 'align
  "Level to indent continuation of enumeration-type environments.

I.e., this affects \\item, \\enumerate, and \\description.

Set this to `align' for:

  \\item lines aligned
         like this.

Set to `auto' for continuation lines to be offset by `LaTeX-indent-line':

  \\item lines aligned
    like this, assuming `LaTeX-indent-line' == 2

Any other fixed integer will be added to `LaTeX-item-indent' and the current
indentation level.

Set this to `nil' to disable all this behavior.

You'll need to adjust `LaTeX-item-indent' to control indentation of \\item
itself.")

;;;###autoload
(defvar +latex-enable-unicode-math nil
  "If non-nil, use `company-math-symbols-unicode' backend in `LaTeX-mode',
enabling unicode symbols in math regions. This requires the unicode-math LaTeX
package to be installed.")

;;;###autoload
(defvar +latex-viewers '(skim evince sumatrapdf zathura okular pdf-tools)
  "A list of enabled LaTeX viewers to use, in this order. If they don't exist,
they will be ignored. Recognized viewers are skim, evince, sumatrapdf, zathura,
okular and pdf-tools.

If no viewer is found, `latex-preview-pane-mode' is used.")
