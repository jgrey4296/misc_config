;; NOTE needs to be recompiled for changes to take effect to bindings

;;----------------------------------------
(defun +jg-personal-ac-trigger ()
  (interactive)
  (auto-complete)
  )
;;--------------------------------------------------
(defun +jg-personal-flatten (lst)
  """ Utility to flatten a list """
  (letrec ((internal (lambda (x)
                       (cond
                        ((null x) nil)
                        ((atom x) (list x))
                        (t
                         (append (funcall internal (car x)) (funcall internal (cdr x))))))))
    (progn
      (assert (listp lst))
      (funcall internal lst))))
(defun +jg-personal-line-starts-with? (text)
  (s-starts-with? text (s-trim-left (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
  )
(defun +jg-personal-split-tags()
  (interactive)
  (goto-char (point-min))
  (let ((letter ?a)
        (end-letter (+ 1 ?z))
        (beg (point-min))
        (fst t)
        subs)
    (while (and (not (equal letter end-letter))
                (re-search-forward (format "^%s" (char-to-string letter)) nil nil))
      (setq subs (buffer-substring beg (- (point) 1)))
      (with-output-to-temp-buffer (if fst "misc.tags" (format "%s.tags" (char-to-string (- letter 1))))
        (princ subs)
        )
      (setq beg (- (point) 1)
            letter (+ letter 1)
            fst nil)
      )
    (setq subs (buffer-substring (- (point) 1) (point-max)))
    (with-output-to-temp-buffer "z.tags"
      (princ subs)
      )
    )
  )
(defun +jg-personal-what-face (pos)
  ;; from: http://stackoverflow.com/questions/1242352/
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
(defun +jg-personal-face-under-cursor-customize (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (customize-face face) (message "No face at %d" pos))))
;;----------------------------------------
;; Goto Buffers

(defun +jg-personal-modify-line-end-display-table ()
  (interactive)
  " from https://stackoverflow.com/questions/8370778/ "
  ;; Modify the display table for whitespace, so lines which
  ;; truncate are not signaled with a $
  (set-display-table-slot standard-display-table 0 ?\ )
  )

(defun +jg-personal-toggle-docstrings ()
  (interactive)
  (setq which-key-show-docstrings
        (if which-key-show-docstrings
            nil
          'docstring-only
            )
        )
  )
;;----------------------------------------
(defun +jg-personal-extend-python-map ()
  (map!
   :map python-mode-map
   :n "z d" '+jg-personal-toggle-all-defs
   :n "z C" '+jg-personal-close-class-defs
   :leader
   (:prefix "i"
    :n "d" #'+jg-personal-python-toggle-breakpoint
    )
   (:prefix "j"
    :n "C" 'helm-gtags-create-tags
    :n "d" 'helm-gtags-find-tag
    :n "D" 'helm-gtags-find-tag-other-window
    :n "G" 'helm-gtags-dwim-other-window
    :n "i" 'helm-gtags-tags-in-this-function
    :n "l" 'helm-gtags-parse-file
    :n "n" 'helm-gtags-next-history
    :n "p" 'helm-gtags-previous-history
    :n "r" 'helm-gtags-find-rtag
    :n "R" 'helm-gtags-resume
    :n "s" 'helm-gtags-select
    :n "S" 'helm-gtags-show-stack
    :n "y" 'helm-gtags-find-symbol
    :n "U" 'helm-gtags-update-tags
    )
   )
)
