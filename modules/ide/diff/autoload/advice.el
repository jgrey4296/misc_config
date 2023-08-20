;;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vc-gutter--shrink-popup-a (fn &rest args)
  "FIX: The revert popup consumes 50% of the frame, whether or not you're
     reverting 2 lines or 20. This resizes the popup to match its contents."
  (letf! ((refine-mode diff-auto-refine-mode)
          (diff-auto-refine-mode t)
          (defun diff-refine-hunk ()
            (when refine-mode
              (funcall diff-refine-hunk))
            (shrink-window-if-larger-than-buffer)))
    (apply fn args))
  )

;;;###autoload
(advice-add 'diff-hl-revert-hunk-1 :around #'+vc-gutter--shrink-popup-a)

;;;###autoload
(defun +vc-gutter--save-excursion-a (fn &rest args)
  "Suppresses unexpected cursor movement by `diff-hl-revert-hunk'.
   FIX: Reverting a hunk causes the cursor to be moved to an unexpected place,
   often far from the target hunk. "
  (let ((pt (point)))
    (prog1 (apply fn args)
      (goto-char pt)))
  )

;;;###autoload
(advice-add 'diff-hl-revert-hunk :around #'+vc-gutter--save-excursion-a)
