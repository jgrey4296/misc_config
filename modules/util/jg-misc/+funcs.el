;;; util/jg-misc/+funcs.el -*- lexical-binding: t; -*-


(defun +jg-misc-open-scratch-buffer (&optional arg)
  "Customised doom/open-project-scratch-buffer because it doesn't use pop-to-buffer "
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall #'pop-to-buffer
     (doom-scratch-buffer
      arg
      (cond ((eq doom-scratch-initial-major-mode t)
             (unless (or buffer-read-only
                         (derived-mode-p 'special-mode)
                         (string-match-p "^ ?\\*" (buffer-name)))
               major-mode))
            ((null doom-scratch-initial-major-mode)
             nil)
            ((symbolp doom-scratch-initial-major-mode)
             doom-scratch-initial-major-mode))
      default-directory
        (doom-project-name)))))
(defun +jg-misc-ivy-predicate (x)
  ;; return nil for cruft buffers
  (not (string-match jg-misc-ivy-predicate-patterns (car x)))
  )
(defun +jg-misc-ivy-switch-buffer ()
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :predicate #'+jg-misc-ivy-predicate
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :sort t
            :caller 'ivy-switch-buffer)
  )
(defun +jg-misc-get-modes ()
  (let (major minor)
    ;; Modes in auto mode alist:
    (loop for mode in (mapcar 'cdr auto-mode-alist)
          do
          (unless (consp mode)
            (pushnew mode major)))

    (loop for mode in (mapcar 'cdr auto-minor-mode-alist)
          do
          (unless (consp mode)
            (pushnew mode minor)))

    ;; modes from packages:
    (loop for pkg in (mapcar 'car (doom-package-list))
          do
          (cond ((string-match "-minor-mode$" (symbol-name pkg))
                 (pushnew pkg minor))
                ((fboundp (intern (format "%s-minor-mode" pkg)))
                 (pushnew (intern (format "%s-minor-mode" pkg)) minor))
                ((string-match "-mode$"  (symbol-name pkg))
                 (pushnew pkg major))
                ((fboundp (intern (format "%s-mode" pkg)))
                 (pushnew (intern (format "%s-mode" pkg)) major))
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
(defun +jg-misc-ivy-rps-transformer (x)
  " Cleans a Candidate line for display  "
  (if (string-match "\.com/\\([0-9/]+\\)/have-you-played-\\(.+?\\)/" x)
      `(,(format "%s : %s" (match-string 1 x)
                 (s-replace "-" " " (match-string 2 x)))
        . ,x)
    `(,x . ,x)
    )
  )
(defun +jg-misc-helm-rps-have-you-playeds ()
  (interactive)
  (let* ((target jg-misc-rps-have-you-played-loc)
         (source (helm-build-in-file-source "Have You Played Helm" target
                   :candidate-transformer (lambda (x)
                                            (mapcar #'+jg-misc-ivy-rps-transformer x))
                   :action (helm-make-actions "Open" #'(lambda (x) (mapcar #'+jg-browse-url (helm-marked-candidates))))
                   )))
    (helm :sources (list source)
          :buffer "*helm have you played*")
    )

  )
(defun +jg-misc-helm-xkcd ()
  " TODO transformers "
  (interactive)
  (let* ((target "/Volumes/documents/github/writing/resources/bibliography_plus/xkcds")
         (source (helm-build-in-file-source "xkcd helm" target
                   :action (helm-make-actions "Open" #'(lambda (x) (mapcar #'+jg-browse-url (helm-marked-candidates))))
                   )))
    (helm :sources (list source)
          :buffer "*helm xkcd*")
    )
  )

(define-advice projectile-run-compilation (:filter-args (val)
                                           +jg-misc-command-expander)
  " Expand variables mentioned in the command "
  (let ((loc (if (eq major-mode 'dired-mode)
                 (dired-current-directory)
               (f-parent (buffer-file-name)))))
    (list (s-replace "\$" (format "dir=\"%s\"" loc) (car val)))
    )
  )


(defun +jg-personal-flatten (lst)
  " Utility to flatten a list "
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
