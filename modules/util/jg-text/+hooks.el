;;; +hooks.el -*- lexical-binding: t; -*-

(defun +jg-text-auto-hide ()
  " Add auto-hiding on buffer open.
Vimish-fold's any blocks matching jg-fold-block-gen's :re pattern
"
  (if vimish-fold-mode
    (save-excursion
      (message "Running Auto Hide: %s %s" major-mode comment-start)
      (beginning-of-buffer)
      (vimish-fold-delete-all)
      (evil-open-folds)
      ;; Fold Groups
      (message "Searching for Fold Blocks")
      (while (re-search-forward (+jg-fold-block-gen :re t) nil t)
        (let* ((group-name (match-string 1))
               start-hide end-hide)
          (cond ((and (s-matches? "^end" group-name)
                      (not start-hide))
                 (message "Found an End Block Too Early"))
                ((s-matches? "^end" group-name)
                 nil)
                (t
                 (setq start-hide (progn (beginning-of-line) (point))
                       end-hide (if (re-search-forward (+jg-fold-block-gen :name group-name :end t :re t) nil t)
                                    (progn (end-of-line) (point))
                                  (message "Couldn't find: %s" end-re)))))

          (if (and start-hide end-hide (not (vimish-fold--folds-in start-hide end-hide)))
              (progn (message "Folding: %s %s %s" group-name start-hide end-hide)
                     (vimish-fold start-hide end-hide)))
          (forward-line)
          )
        )
      )
    )
  )
