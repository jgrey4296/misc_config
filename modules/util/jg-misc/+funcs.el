;;; util/jg-misc/+funcs.el -*- lexical-binding: t; -*-

(defun +jg-misc-browse-url (&optional url)
  (interactive)
  (let ((url (cond (url url)
                   ((eq evil-state 'visual)
                    (buffer-substring-no-properties evil-visual-beginning evil-visual-end))
                   (t
                    (read-string "Search For: "))
                   ))
        )
    (cond ((f-exists? url)
           (shell-command (format "open %s" url)))
          ((s-prefix? "@" url)
           (browse-url (format "https://twitter.com/%s" url)))
          ((s-prefix? "http" url)
           (browse-url url))
          ((s-prefix? "www." url)
           (browse-url (format "https://%s" url)))
          ((string-match "\\.com\\|\\.uk" url)
           (browse-url (format "https://%s" url)))
          (t
           (message "Browsing for: %s" (format jg-misc-google-url url))
           (browse-url (format jg-misc-google-url url)))
          )
    )
  )

(defun +jg-misc-get-modes ()
  (let (major minor)
    ;; Modes in auto mode alist:
    (cl-loop for mode in (mapcar 'cdr auto-mode-alist)
          do
          (unless (consp mode)
            (cl-pushnew mode major)))

    (cl-loop for mode in (mapcar 'cdr auto-minor-mode-alist)
          do
          (unless (consp mode)
            (cl-pushnew mode minor)))

    ;; modes from packages:
    (cl-loop for pkg in (mapcar 'car (doom-package-list))
          do
          (cond ((string-match "-minor-mode$" (symbol-name pkg))
                 (cl-pushnew pkg minor))
                ((fboundp (intern (format "%s-minor-mode" pkg)))
                 (cl-pushnew (intern (format "%s-minor-mode" pkg)) minor))
                ((string-match "-mode$"  (symbol-name pkg))
                 (cl-pushnew pkg major))
                ((fboundp (intern (format "%s-mode" pkg)))
                 (cl-pushnew (intern (format "%s-mode" pkg)) major))
                (t nil)
                )
          )

    (list major minor)
    )
  )
(defun +jg-misc-sync-movements ()
  ;; TODO
  ;; Get current windows
  ;; add advice to evil line move

  )

(defun +jg-misc-flatten (lst)
  " Utility to flatten a list "
  (letrec ((internal (lambda (x)
                       (cond
                        ((null x) nil)
                        ((atom x) (list x))
                        (t
                         (append (funcall internal (car x)) (funcall internal (cdr x))))))))
    (progn
      (cl-assert (listp lst))
      (funcall internal lst))))
(defun +jg-misc-line-starts-with? (text)
  (s-starts-with? text (s-trim-left (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
  )
(defun +jg-misc-split-tags()
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
(defun +jg-misc-what-face (pos)
  ;; from: http://stackoverflow.com/questions/1242352/
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun +jg-misc-face-under-cursor-customize (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (customize-face face) (message "No face at %d" pos))))
(defun +jg-misc-modify-line-end-display-table ()
  (interactive)
  " from https://stackoverflow.com/questions/8370778/ "
  ;; Modify the display table for whitespace, so lines which
  ;; truncate are not signaled with a $
  (set-display-table-slot standard-display-table 0 ?\ )
  )
(defun +jg-misc-toggle-docstrings ()
  (interactive)
  (setq which-key-show-docstrings
        (if which-key-show-docstrings
            nil
          'docstring-only
            )
        )
  )
