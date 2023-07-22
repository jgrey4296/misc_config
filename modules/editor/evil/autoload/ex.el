;;; editor/evil/autoload/ex.el -*- lexical-binding: t; -*-

(defvar +evil--flag nil)

(defun +evil--ex-match-init (name &optional face update-hook)
  (with-current-buffer evil-ex-current-buffer
    (cond
     ((eq +evil--flag 'start)
      (evil-ex-make-hl name
                       :face (or face 'evil-ex-lazy-highlight)
                       :update-hook (or update-hook #'evil-ex-pattern-update-ex-info))
      (setq +evil--flag 'update))

     ((eq +evil--flag 'stop)
      (evil-ex-delete-hl name)))))

(defun +evil--ex-buffer-match (arg &optional hl-name flags beg end)
  (when (and (eq +evil--flag 'update)
             evil-ex-substitute-highlight-all
             (not (zerop (length arg))))
    (condition-case lossage
        (let* ((pattern (evil-ex-make-substitute-pattern
                         arg
                         (or flags (list))))
               (range (or (evil-copy-range evil-ex-range)
                          (evil-range (or beg (line-beginning-position))
                                      (or end (line-end-position))
                                      'line
                                      :expanded t))))
          (evil-expand-range range)
          (evil-ex-hl-set-region hl-name
                                 (max (evil-range-beginning range) (window-start))
                                 (min (evil-range-end range) (window-end)))
          (evil-ex-hl-change hl-name pattern))
      (end-of-file
       (evil-ex-pattern-update-ex-info nil "incomplete replacement"))
      (user-error
       (evil-ex-pattern-update-ex-info nil (format "?%s" lossage))))))

;;;###autoload
(defun +evil-ex-regexp-match (flag &optional arg invert)
  (let ((hl-name 'evil-ex-buffer-match)
        (+evil--flag flag))
    (with-selected-window (minibuffer-selected-window)
      (+evil--ex-match-init hl-name)
      (cl-destructuring-bind (&optional arg flags)
          (evil-delimited-arguments arg 2)
        (let ((evil-ex-substitute-global
               (if invert
                   (not evil-ex-substitute-global)
                 evil-ex-substitute-global)))
          (+evil--ex-buffer-match
           arg hl-name (string-to-list flags)))))))

;;;###autoload (autoload '+evil:compile "editor/evil/autoload/ex" nil t)
(evil-define-command +evil:compile (arguments &optional bang)
  "Run `compile-command' with ARGUMENTS.
If BANG is non-nil, open compilation output in a comint buffer.

This command understands vim file modifiers (like %:p:h). See
`+evil-replace-filename-modifiers-a' for details."
  (interactive "<sh><!>")
  (compile (evil-ex-replace-special-filenames
            (format "%s %s"
                    (eval compile-command)
                    arguments))
           bang))

;;;###autoload (autoload '+evil:help "editor/evil/autoload/ex" nil t)
(evil-define-command +evil:help (&optional bang query)
  "Look up documentation for QUERY.

If QUERY is in the format of an ex command, it will map it to the underlying
function and open its documentation with `helpful-function'. Otherwise, it will
search for it with `apropos'.

If QUERY is empty, this runs the equivalent of 'M-x apropos'. If BANG is
non-nil, a search is preformed against Doom's manual (with
`doom/help-search-headings')."
  (interactive "<!><a>")
  (if bang
      (doom/help-search-headings query)
    (save-match-data
      (cond ((or (null query) (string-empty-p (string-trim query)))
             (call-interactively
              (or (command-remapping #'apropos)
                  #'apropos)))
            ((string-match "^ *:\\([^ ]+\\)$" query)
             (helpful-function
              (evil-ex-completed-binding (match-string 1 query))))
            ((message "Searching for %S, this may take a while..." query)
             (apropos query t))))))

;;;###autoload (autoload '+evil:swiper "editor/evil/autoload/ex" nil t)
(evil-define-command +evil:swiper (&optional search)
  "Invoke `swiper' with SEARCH, otherwise with the symbol at point."
  (interactive "<a>")
  (swiper-isearch search))

;;;###autoload (autoload '+evil:pwd "editor/evil/autoload/ex" nil t)
(evil-define-command +evil:pwd (bang)
  "Display the current working directory. If BANG, copy it to your clipboard."
  (interactive "<!>")
  (if (not bang)
      (pwd)
    (kill-new default-directory)
    (message "Copied to clipboard")))

;;;###autoload (autoload '+jg-evil-list-ex-commands "editor/evil/autoload/ex" nil t)
(evil-define-command +jg-evil-list-ex-commands (&rest args)
  "List available ex commands"
  (interactive)
  (with-temp-buffer-window "*Ex-Commands*" #'+popup-buffer nil
    (princ (string-join (mapcar
                         #'(lambda (x)
                             (format "%-8s : %s"
                                     (car x)
                                     (-if-let (doc (documentation (cdr x)))
                                         (car (split-string doc "\n"))
                                       (cdr x)))
                             )
                         evil-ex-commands) "\n"))
    )
  )
