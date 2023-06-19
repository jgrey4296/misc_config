;;; editor/fold/autoload/fold.el -*- lexical-binding: t; -*-

;; `hideshow' is a decent code folding implementation, but it won't let you
;; create custom folds. `vimish-fold' offers custom folds, but essentially
;; ignores any other type of folding (indent or custom markers, which hideshow
;; and `outline-mode' give you). This is my effort to combine them.

;;
;;; Helpers

(defun +fold--ensure-hideshow-mode ()
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode +1)))

(defun +fold--vimish-fold-p ()
  (and (featurep 'vimish-fold)
       (cl-some #'vimish-fold--vimish-overlay-p
                (overlays-at (point)))))

(defun +fold--outline-fold-p ()
  (and (or (bound-and-true-p outline-minor-mode)
           (derived-mode-p 'outline-mode))
       (outline-on-heading-p)))

(defun +fold--hideshow-fold-p ()
  (+fold--ensure-hideshow-mode)
  (save-excursion
    (ignore-errors
      (or (hs-looking-at-block-start-p)
          (hs-find-block-beginning)
          (unless (eolp)
            (end-of-line)
            (+fold--hideshow-fold-p))))))

(defun +fold--invisible-points (count)
  (let (points)
    (save-excursion
      (catch 'abort
        (if (< count 0) (beginning-of-line))
        (while (re-search-forward hs-block-start-regexp nil t
                                  (if (> count 0) 1 -1))
          (unless (invisible-p (point))
            (end-of-line)
            (when (hs-already-hidden-p)
              (push (point) points)
              (when (>= (length points) count)
                (throw 'abort nil))))
          (forward-line (if (> count 0) 1 -1)))))
    points))

(defmacro +fold-from-eol (&rest body)
  "Perform action after moving to the end of the line."
  `(save-excursion
     (end-of-line)
     ,@body))

;;
;;; Commands

;;;###autoload
(defun +fold/toggle ()
  "Toggle the fold at point.

Targets `vimmish-fold', `hideshow' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--outline-fold-p) (outline-toggle-children))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-toggle-hiding))))))

;;;###autoload
(defun +fold/open ()
  "Open the folded region at point.

Targets `vimmish-fold', `hideshow' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--outline-fold-p)
           (outline-show-children)
           (outline-show-entry))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-show-block))))))

;;;###autoload
(defun +fold/close ()
  "Close the folded region at point.

Targets `vimmish-fold', `hideshow' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--hideshow-fold-p) (+fold-from-eol (hs-hide-block)))
          ((+fold--outline-fold-p) (outline-hide-subtree)))))

;;;###autoload
(defun +fold/open-all (&optional level)
  "Open folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (save-excursion
    (goto-char (point-min))
    (if (integerp level)
        (progn
          (message "Level %s" level)
          (outline-show-all)
          (hs-life-goes-on
           (hs-hide-level-recursive level (point-min) (point-max))))
      (hs-life-goes-on (hs-show-all))
      (outline-show-all))))

;;;###autoload
(defun +fold/close-all (&optional level)
  "Close folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (save-excursion
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (outline-next-visible-heading 1)
      (outline-hide-subtree))
    (hs-life-goes-on
     (if (integerp level)
         (hs-hide-level-recursive (- level 1) (point-min) (point-max))
       (hs-hide-all)))))

;;;###autoload
(defun +fold/next (count)
  "Jump to the next vimish fold, outline heading or folded region."
  (interactive "p")
  (cl-loop with orig-pt = (point)
           for fn
           in (list (lambda ()
                      (when (bound-and-true-p hs-block-start-regexp)
                        (car (+fold--invisible-points count))))
                    (lambda ()
                      (when (featurep 'vimish-fold)
                        (if (> count 0)
                            (evil-vimish-fold/next-fold count)
                          (evil-vimish-fold/previous-fold (- count))))
                      (if (/= (point) orig-pt) (point))))
           if (save-excursion (funcall fn))
           collect it into points
           finally do
           (if-let* ((pt (car (sort points (if (> count 0) #'< #'>)))))
               (goto-char pt)
             (message "No more folds %s point" (if (> count 0) "after" "before"))
             (goto-char orig-pt))))

;;;###autoload
(defun +fold/previous (count)
  "Jump to the previous vimish fold, outline heading or folded region."
  (interactive "p")
  (+fold/next (- count)))


;;;###autoload
(defun +jg-fold/debug ()
  (interactive)
  (message "Using Fold Spec: %s" (cl-loop for spec in evil-fold-list
                                          if (evil--mode-p (ensure-list (caar spec)))
                                          return spec
                                          else do '(nil)
                                          )
           )
  )
