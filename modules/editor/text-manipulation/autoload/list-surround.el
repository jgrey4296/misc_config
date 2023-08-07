;;; list-surround.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+jg-surround-list "editor/text-manipulation/autoload/list-surround.el" nil t)
(evil-define-operator +jg-surround-list (beg end args)
  :type inclusive
  :motion nil
  (interactive "<R>")
  (let* ((list-pair (evil-surround-pair (read-char "List Type: "))))
    (save-excursion
      (goto-char end)
      (if (looking-at-p "\n")
          (backward-char 1))
      (insert (cdr list-pair))
      (goto-char beg)
      (insert (car list-pair))
      (unless (derived-mode-p 'emacs-lisp-mode)
        (+jg-surround-ensure-commas beg end args))
      )
    )
  )

;;;###autoload (autoload '+jg-surround-ensure-commas  "editor/text-manipulation/autoload/list-surround.el" nil t)
(evil-define-operator +jg-surround-ensure-commas (beg end args)
  :type inclusive
  :keep-visual t
  (interactive "<R>")
  (save-excursion
    (goto-char (max beg end))
    ;; look for blanks and new lines, add commas in between
    (while (re-search-backward (rx word-end (opt (| ?\" ?')) (group (| "\n" blank))) (min beg end) t)
      (goto-char (match-beginning 1))
      (unless (nth 3 (syntax-ppss)) ;; if in string
        (insert ","))
      (goto-char (match-beginning 0))
      )
    )
  )
