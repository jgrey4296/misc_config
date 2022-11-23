;;; +autohide.el -*- lexical-binding: t; -*- no-byte-compile: t; -*-
;;-- requires
(require 'vimish-fold)
(require 's)
(require 'evil)
;;-- end requires

;;-- vars
(defvar-local autohide-minor-mode-fold-pattern "%s-- %s %s")
(defvar-local autohide-minor-mode-block-depth 2)
(defvar autohide-minor-mode-exclusions '(helm-major-mode ivy-mode minibuffer-mode dired-mode fundamental-mode))
;;-- end vars

;;-- main functions
(cl-defun autohide-minor-mode-fold-block-gen ( &rest rst &key (name "\\(.+\\)") (end nil) (re nil) (newlines nil) (comment comment-start))
  " Single point to build fold block markers
Auto-recognizes the current major-mode's commment syntax

keys:
:name     - the name of the block to generate
:end      - generate the end block instead of start
:re       - generate a regex for searching for blocks
:newlines - add a newline to the generated block
:comment  - comment syntax override, defaults to major mode's `comment-start`

 "
  (let* ((comment-str (apply 'concat (make-list autohide-minor-mode-block-depth (s-trim (or comment ";;")))))
         (end-str (when end "end "))
         (name-form (s-concat end-str name))
         (full-pattern (s-trim (format autohide-minor-mode-fold-pattern comment-str name-form comment-end)))
         )
    (cond ((and re newlines)         (error "Fold Block Invalid Combined Args: :re and :newlines"))
          ((and newlines (not name)) (error ":newlines should also have :name"))
          (re       (s-concat "^[[:blank:]]*" full-pattern))
          (newlines (s-concat (if end "\n" "") full-pattern "\n"))
          (t full-pattern))))

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
               (error "Found an End Block Too Early: %s" group-name))
              ((s-matches? "^end" group-name)
               nil)
              (t
               (setq start-hide (line-beginning-position)
                     end-hide (if (re-search-forward (autohide-minor-mode-fold-block-gen :name group-name :end t :re t) nil t)
                                  (line-end-position)
                                (error "Couldn't find: %s" group-name))))
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

;;-- end main functions

;;-- mode definition
(define-minor-mode autohide-minor-mode
  " Minor mode to automatically hide blocks of text upon loading a buffer "
  :init-value nil
  :lighter "autohide"
  (when (and autohide-minor-mode
             (not (minibufferp))
             (not (-contains? autohide-minor-mode-exclusions major-mode))
             (derived-mode-p 'prog-mode)
             )
    (autohide-minor-mode-run-folds))
  )

(define-globalized-minor-mode global-autohide-minor-mode
  autohide-minor-mode autohide-minor-mode)

;;-- end mode definition

;;-- evil operator
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


;;-- end evil operator

;;-- ivy
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

;;-- end ivy

(provide 'autohide-minor-mode)
