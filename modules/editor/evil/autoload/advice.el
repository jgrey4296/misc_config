;;; editor/evil/autoload/advice.el -*- lexical-binding: t; -*-

(defun +evil--insert-newline (&optional above _noextranewline)
  (let ((pos (save-excursion (beginning-of-line-text) (point)))
        comment-auto-fill-only-comments)
    (require 'smartparens)
    (evil-narrow-to-field
      (if above
          (if (save-excursion (nth 4 (sp--syntax-ppss pos)))
              (evil-save-goal-column
                (setq evil-auto-indent nil)
                (goto-char pos)
                (let ((ws (abs (skip-chars-backward " \t"))))
                  ;; FIXME oh god why
                  (save-excursion
                    (if comment-line-break-function
                        (funcall comment-line-break-function nil)
                      (comment-indent-new-line))
                    (when (and (derived-mode-p 'c-mode 'c++-mode 'objc-mode 'java-mode 'js2-mode)
                               (eq (char-after) ?/))
                      (insert "*"))
                    (insert
                     (make-string (max 0 (+ ws (skip-chars-backward " \t")))
                                  32)))
                  (insert (make-string (max 1 ws) 32))))
            (evil-move-beginning-of-line)
            (insert (if use-hard-newlines hard-newline "\n"))
            (forward-line -1)
            (back-to-indentation))
        (evil-move-end-of-line)
        (cond ((sp-point-in-comment pos)
               (setq evil-auto-indent nil)
               (if comment-line-break-function
                   (funcall comment-line-break-function nil)
                 (comment-indent-new-line)))
              ;; TODO Find a better way to do this
              ((and (eq major-mode 'haskell-mode)
                    (fboundp 'haskell-indentation-newline-and-indent))
               (setq evil-auto-indent nil)
               (haskell-indentation-newline-and-indent))
              (t
               (insert (if use-hard-newlines hard-newline "\n"))
               (back-to-indentation)))))))

;;;###autoload
(defun +evil-escape-a (&rest _)
  "Call `doom/escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'doom/escape)))

;;;###autoload
(defun +evil-replace-filename-modifiers-a (file-name)
  "Take a path and resolve any vim-like filename modifiers in it. This adds
support for most vim file modifiers, as well as:

  %:P   Resolves to `doom-project-root'.

See http://vimdoc.sourceforge.net/htmldoc/cmdline.html#filename-modifiers for
more information on modifiers."
  (let ((origin-buffer (current-buffer))
        case-fold-search)
    (with-temp-buffer
      (let ((buffer-file-name (buffer-file-name origin-buffer)))
        (save-excursion (insert file-name))
        (while (re-search-forward "\\(^\\|[^\\\\]\\)\\(\\([%#]\\)\\(:\\([PphtreS~.]\\|g?s\\)\\)*\\)" nil t)
          (if (null buffer-file-name)
              (replace-match (match-string 1) t t nil 2)
            (let ((beg (match-beginning 2))
                  (end (match-end 3))
                  (path (pcase (match-string 3)
                          ("%" (file-relative-name buffer-file-name default-directory))
                          ("#" (and (other-buffer origin-buffer)
                                    (buffer-file-name (other-buffer origin-buffer)))))))
              (save-match-data
                (goto-char beg)
                (while (re-search-forward ":\\([PphtreS~.]\\|g?s\\)" (+ (point) 3) t)
                  (let* ((modifier (match-string 1))
                         (global (string-prefix-p "gs" modifier)))
                    (when global
                      (setq modifier (substring modifier 1)))
                    (setq end (match-end 1)
                          path
                          (pcase (and path (substring modifier 0 1))
                            (`nil "")
                            ("p" (expand-file-name path))
                            ("~" (concat "~/" (file-relative-name path "~")))
                            ("." (file-relative-name path))
                            ("t" (file-name-nondirectory (directory-file-name path)))
                            ("r" (file-name-sans-extension path))
                            ("e" (file-name-extension path))
                            ("S" (shell-quote-argument path))
                            ("h"
                             (let ((parent (file-name-directory (expand-file-name path))))
                               (unless (file-equal-p path parent)
                                 (if (file-name-absolute-p path)
                                     (directory-file-name parent)
                                   (file-relative-name parent)))))
                            ("s"
                             (if (featurep 'evil)
                                 (when-let (args (evil-delimited-arguments (substring modifier 1) 2))
                                   (let ((pattern (evil-transform-vim-style-regexp (car args)))
                                         (replace (cadr args)))
                                     (replace-regexp-in-string
                                      (if global pattern (concat "\\(" pattern "\\).*\\'"))
                                      (evil-transform-vim-style-regexp replace) path t t
                                      (unless global 1))))
                               path))
                            ("P"
                             (let ((project-root (doom-project-root (file-name-directory (expand-file-name path)))))
                               (unless project-root
                                 (user-error "Not in a project"))
                               (abbreviate-file-name project-root)))))
                    ;; strip trailing slash, if applicable
                    (or (string-empty-p path)
                        (not (equal (substring path -1) "/"))
                        (setq path (substring path 0 -1))))))
              (replace-match path t t nil 2))))
        (replace-regexp-in-string "\\\\\\([#%]\\)" "\\1" (buffer-string) t)))))

;;;###autoload
(defun +evil--insert-newline-below-and-respect-comments-a (fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-below))
          (evil-insert-state-p)
          (evil-emacs-state-p))
      (funcall fn count)
    (letf! (defun evil-insert-newline-below () (+evil--insert-newline))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall fn count)))))

;;;###autoload
(defun +evil--insert-newline-above-and-respect-comments-a (fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-above))
          (evil-insert-state-p)
          (evil-emacs-state-p))
      (funcall fn count)
    (letf! (defun evil-insert-newline-above () (+evil--insert-newline 'above))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall fn count)))))

;;;###autoload (autoload '+evil-window-split-a "editor/evil/autoload/advice" nil t)
(evil-define-command +evil-window-split-a (&optional count file)
  "Same as `evil-window-split', but correctly updates the window history."
  :repeat nil
  (interactive "P<f>")
  ;; HACK This ping-ponging between the destination and source windows is to
  ;;      update the window focus history, so that, if you close either split
  ;;      afterwards you won't be sent to some random window.
  (let ((origwin (selected-window))
        window-selection-change-functions)
    (select-window (split-window origwin count 'below))
    (unless evil-split-window-below
      (select-window origwin)))
  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

;;;###autoload (autoload '+evil-window-vsplit-a "editor/evil/autoload/advice" nil t)
(evil-define-command +evil-window-vsplit-a (&optional count file)
  "Same as `evil-window-split', but correctly updates the window history."
  :repeat nil
  (interactive "P<f>")
  ;; HACK This ping-ponging between the destination and source windows is to
  ;;      update the window focus history, so that, if you close either split
  ;;      afterwards you won't be sent to some random window.
  (let ((origwin (selected-window))
        window-selection-change-functions)
    (select-window (split-window origwin count 'right))
    (unless evil-vsplit-window-right
      (select-window origwin)))
  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

;;;###autoload (autoload '+evil-join-a "editor/evil/autoload/advice" nil nil)
(defun +evil-join-a (fn beg end)
  "Join the selected lines.

This advice improves on `evil-join' by removing comment delimiters when joining
commented lines, without `fill-region-as-paragraph'.

Adapted from https://github.com/emacs-evil/evil/issues/606"
  (if-let* (((not (= (line-end-position) (point-max))))
            (cend (save-excursion (goto-char end) (line-end-position)))
            (cbeg (save-excursion
                    (goto-char beg)
                    (and (doom-point-in-comment-p
                          (save-excursion
                            (goto-char (line-beginning-position 2))
                            (skip-syntax-forward " \t")
                            (point)))
                         (or (comment-search-backward (line-beginning-position) t)
                             (comment-search-forward  (line-end-position) t)
                             (and (doom-point-in-comment-p beg)
                                  (stringp comment-continue)
                                  (or (search-forward comment-continue (line-end-position) t)
                                      beg)))))))
      (let* ((count (count-lines beg end))
             (count (if (> count 1) (1- count) count))
             (fixup-mark (make-marker)))
        (uncomment-region (line-beginning-position 2)
                          (save-excursion
                            (goto-char cend)
                            (line-end-position 0)))
        (unwind-protect
            (dotimes (_ count)
              (join-line 1)
              (save-match-data
                (when (or (and comment-continue
                               (not (string-empty-p comment-continue))
                               (looking-at (concat "\\(\\s-*" (regexp-quote comment-continue) "\\) ")))
                          (and comment-start-skip
                               (not (string-empty-p comment-start-skip))
                               (looking-at (concat "\\(\\s-*" comment-start-skip "\\)"))))
                  (replace-match "" t nil nil 1)
                  (just-one-space))))
          (set-marker fixup-mark nil)))
    ;; But revert to the default we're not in a comment, where
    ;; `fill-region-as-paragraph' is too greedy.
    (funcall fn beg end)))

;;;###autoload
(defun +evil--fix-dabbrev-in-minibuffer-h ()
  "Make `try-expand-dabbrev' from `hippie-expand' work in minibuffer. See
`he-dabbrev-beg', so we need to redefine syntax for '/'."
  (set-syntax-table (let* ((table (make-syntax-table)))
                      (modify-syntax-entry ?/ "." table)
                      table)))

;; Start help-with-tutorial in emacs state
;;;###autoload
(advice-add 'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

;;;###autoload
(defun +evil--dont-move-cursor-a (fn &rest args)
  " HACK '=' moves the cursor to the beginning of selection. Disable this,
      since it's more disruptive than helpful. "
  (save-excursion (apply fn args))
  )

;;;###autoload
(advice-add 'evil-indent :around #'+evil--dont-move-cursor-a)

;;;###autoload
(defun +evil--make-numbered-markers-global-a (char)
  " REVIEW In evil, registers 2-9 are buffer-local. In vim, they're global,
    so... Perhaps this should be PRed upstream? "
  (and (>= char ?2) (<= char ?9)))

;;;###autoload
(advice-add 'evil-global-marker-p :after-until #'+evil--make-numbered-markers-global-a)

;;;###autoload
(defun +evil--fix-local-vars-a (&rest _)
  " REVIEW Fix #2493: dir-locals cannot target fundamental-mode when evil-mode
    is active. See hlissner/doom-emacs#2493. Revert this if
    emacs-evil/evil#1268 is resolved upstream. "
  (when (eq major-mode 'fundamental-mode)
    (hack-local-variables)))

;;;###autoload
(advice-add 'turn-on-evil-mode :before #'+evil--fix-local-vars-a)

;;;###autoload
(defun +evil--fix-helpful-key-in-evil-ex-a (key-sequence)
  " HACK Invoking helpful from evil-ex throws a 'No recursive edit is in
        progress' error because, between evil-ex and helpful,
       `abort-recursive-edit' gets called one time too many. "
  (when (evil-ex-p)
    (run-at-time 0.1 nil #'helpful-key key-sequence)
    (abort-recursive-edit)))

;;;###autoload
(advice-add 'helpful-key :before #'+evil--fix-helpful-key-in-evil-ex-a)

;; Make J (evil-join) remove comment delimiters when joining lines.
;;;###autoload
(advice-add #'evil-join :around #'+evil-join-a)

;;;###autoload
(defun +evil--no-squeeze-on-fill-a (fn &rest args)
  " Prevent gw (`evil-fill') and gq (`evil-fill-and-move') from squeezing
        spaces. It doesn't in vim, so it shouldn't in evil. "

  (letf! (defun fill-region (from to &optional justify nosqueeze to-eop)
           (funcall fill-region from to justify t to-eop))
    (apply fn args)))

;;;###autoload
(advice-add 'evil-fill :around #'+evil--no-squeeze-on-fill-a)

;;;###autoload
(advice-add 'evil-fill-and-move :around #'+evil--no-squeeze-on-fill-a)

;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
;;;###autoload
(advice-add 'evil-force-normal-state :after #'+evil-escape-a)

;; monkey patch `evil-ex-replace-special-filenames' to improve support for
;; file modifiers like %:p:h. This adds support for most of vim's modifiers,
;; and one custom one: %:P (expand to the project root).
;;;###autoload
(advice-add 'evil-ex-replace-special-filenames :override #'+evil-replace-filename-modifiers-a)

;; make `try-expand-dabbrev' (from `hippie-expand') work in minibuffer
;;;###autoload
(add-hook 'minibuffer-inactive-mode-hook #'+evil--fix-dabbrev-in-minibuffer-h)

;; Focus and recenter new splits
;;;###autoload
(advice-add 'evil-window-split  :override #'+evil-window-split-a)
;;;###autoload
(advice-add 'evil-window-vsplit :override #'+evil-window-vsplit-a)

;; Make o/O continue comments (see `+evil-want-o/O-to-continue-comments' to disable)
;;;###autoload
(advice-add 'evil-open-above :around #'+evil--insert-newline-above-and-respect-comments-a)
;;;###autoload
(advice-add 'evil-open-below :around #'+evil--insert-newline-below-and-respect-comments-a)

;;;###autoload
(defun +jg-evil-iedit-show-all ()
    " Override iedit's show all so it doesn't mess with invisible line movement"
    (remove-from-invisibility-spec '(iedit-invisible-overlay-name . t))
    (remove-overlays nil nil iedit-invisible-overlay-name t)
  )

;;;###autoload
(advice-add 'iedit-show-all :override #'+jg-evil-iedit-show-all)

;;;###autoload
(defun +jg-evil-marks-cleanup (marks)
  (list (-filter #'(lambda (x) (and x
                                    (marker-position x)
                                    (marker-buffer x)))
                 (car marks))))

;;;###autoload
(advice-add 'counsel-mark--get-candidates :filter-args #'+jg-evil-marks-cleanup)

;;;###autoload (autoload '+jg-evil-find-char "editor/evil/autoload/advice.el" nil t)
(evil-define-motion +jg-evil-find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR.
Movement is restricted to the current line unless `evil-cross-lines' is non-nil.
JG: Modified to respect case-fold-search
"
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (visual (and evil-respect-visual-line-mode
                     visual-line-mode))
        )
    (setq evil-last-find (list #'evil-find-char char fwd))
    (when fwd (evil-forward-char 1 evil-cross-lines))
    (unless (prog1
                (search-forward
                 (char-to-string char)
                 (cond (evil-cross-lines nil)
                       ((and fwd visual)
                        (save-excursion
                          (end-of-visual-line)
                          (point)))
                       (fwd (line-end-position))
                       (visual
                        (save-excursion
                          (beginning-of-visual-line)
                          (point)))
                       (t (line-beginning-position)))
                 t count)
              (when fwd (backward-char)))
      (user-error "Can't find `%c'" char))))

;;;###autoload
(advice-add 'evil-find-char :override #'+jg-evil-find-char)
