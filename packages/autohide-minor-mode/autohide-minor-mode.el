;;; +autohide.el -*- lexical-binding: t; -*- no-byte-compile: t; -*-
(require 'vimish-fold)
(require 's)
(require 'evil)

(defvar-local autohide-minor-mode-fold-pattern "%s-- %s")
(defvar-local autohide-minor-mode-block-depth 2)
(defvar autohide-minor-mode-exclusions '(helm-major-mode ivy-mode minibuffer-mode ))

(cl-defun autohide-minor-mode-fold-block-gen (&rest rst &key name (end nil) (re nil) (newlines nil) (comment comment-start))
  " Single point to build fold block markers
Auto-recognizes the current major-mode's commment syntax
 "
  (let* ((comment-str (apply 'concat (make-list autohide-minor-mode-block-depth (s-trim (or comment ";;")))))
         (end-str (if end "end " nil))
         (name-pattern "\\(.+\\)")
         (name-form (s-concat end-str (or name name-pattern)))
         (full-pattern (format autohide-minor-mode-fold-pattern comment-str name-form))
         )
    (cond ((and re newlines) (error "Fold Block Invalid Combined Args: :re and :newlines"))
          ((and newlines (not name)) (error ":newlines should also have :name"))
          (re       (s-concat "^[[:blank:]]*" full-pattern))
          (newlines (s-concat (if end "\n" "") full-pattern "\n"))
          (t full-pattern))))
(defun autohide-minor-mode-fold-jump-to-heading ()
  " Ivy to jump to auto-hide sections  "
  (interactive)
  (let (sections)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (autohide-minor-mode-fold-block-gen :re t) nil t)
        (if (not (s-contains? "end " (match-string 1)))
            (push (match-string 1) sections)
          )
        )
      )
    (goto-char (point-min))
    (re-search-forward (autohide-minor-mode-fold-block-gen :name (ivy-read "Heading: " sections)) nil t)
    (beginning-of-line)
    )
  )
(defun autohide-minor-mode-run-folds ()
  " Add auto-hiding on buffer open.
Vimish-fold's any blocks matching autohide-minor-mode-fold-pattern
"
  (interactive)
  (message "Running Auto Hide: %s %s" major-mode comment-start)
  (save-excursion
    (beginning-of-buffer)
    (vimish-fold-delete-all)
    ;; Fold Groups
    (message "Searching for Fold Blocks")
    (while (re-search-forward (autohide-minor-mode-fold-block-gen :re t) nil t)
      (let* ((group-name (match-string 1))
             start-hide end-hide)
        (cond ((and (s-matches? "^end" group-name)
                    (not start-hide))
               (message "Found an End Block Too Early: %s" group-name))
              ((s-matches? "^end" group-name)
               nil)
              (t
               (setq start-hide (progn (beginning-of-line) (point))
                     end-hide (if (re-search-forward (autohide-minor-mode-fold-block-gen :name group-name :end t :re t) nil t)
                                  (progn (end-of-line) (point))
                                (message "Couldn't find: %s" group-name))))
              )

        (when (and start-hide end-hide (not (vimish-fold--folds-in start-hide end-hide)))
          (message "Folding: %s %s %s" group-name start-hide end-hide)
          (vimish-fold start-hide end-hide)
          (goto-char end-hide))
        (forward-line)
        )
      )
    )
  )
(evil-define-operator autohide-minor-mode-wrap-block (beg end type &optional name)
  " Operator to easily create fold blocks "
  :type line
  :keep-visual t
  (interactive "<R>" (list (read-string "Block Name: ")))
  ;; From bottom to top to not change interfere with positions
  ;; add end fold block
  (goto-char end)
  (end-of-line)
  (insert (autohide-minor-mode-fold-block-gen :name name :end t :newlines t))
  ;; and start fold block
  (goto-char beg)
  (beginning-of-line)
  (insert (autohide-minor-mode-fold-block-gen :name name :newlines t))
  )

(define-minor-mode autohide-minor-mode
  " Minor mode to automatically hide blocks of text upon loading a buffer "
  :init-value nil
  :lighter "autohide"
  (if (and autohide-minor-mode (not (minibufferp)) (not (-contains? autohide-minor-mode-exclusions major-mode)))
      (add-hook 'after-change-major-mode-hook #'autohide-minor-mode-run-folds 100 t)
    (remove-hook 'after-change-major-mode-hook #'autohide-minor-mode-run-folds t)
    )
  )

(define-globalized-minor-mode global-autohide-minor-mode
  autohide-minor-mode autohide-minor-mode)


(provide 'autohide-minor-mode)
