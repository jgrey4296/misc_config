;;; +hooks.el -*- lexical-binding: t; -*-

(defun +jg-text-auto-hide ()
  " Add auto-hiding on buffer open.
Vimish-fold's any blocks matching jg-text-fold-block-gen's :re pattern
"
  (message "Running Auto Hide: %s %s" major-mode comment-start)
  (if (and global-evil-vimish-fold-mode jg-text-auto-hide-toggle)
    (save-excursion
      (beginning-of-buffer)
      (vimish-fold-delete-all)
      ;; Fold Groups
      (message "Searching for Fold Blocks")
      (while (re-search-forward (+jg-text-fold-block-gen :re t) nil t)
        (let* ((group-name (match-string 1))
               start-hide end-hide)
          (cond ((and (s-matches? "^end" group-name)
                      (not start-hide))
                 (message "Found an End Block Too Early: %s" group-name))
                ((s-matches? "^end" group-name)
                 nil)
                (t
                 (setq start-hide (progn (beginning-of-line) (point))
                       end-hide (if (re-search-forward (+jg-text-fold-block-gen :name group-name :end t :re t) nil t)
                                    (progn (end-of-line) (point))
                                  (message "Couldn't find: %s" group-name)))))

          (if (and start-hide end-hide (not (vimish-fold--folds-in start-hide end-hide)))
              (progn (message "Folding: %s %s %s" group-name start-hide end-hide)
                     (vimish-fold start-hide end-hide)
                     (goto-char end-hide)
                     (forward-line)))
          (forward-line)
          )
        )
      )
    (message "Skipping Generic Auto Hide")
    )
  )
