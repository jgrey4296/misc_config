(when (configuration-layer/package-usedp 'org)

  (defun jg_org/insert-heading-trio ()
    (interactive)
    (org-insert-subheading 1)
    (insert "1: ")
    (org-insert-heading 3 nil nil)
    (insert "2: ")
    (org-insert-heading 1 nil nil)
    (insert "3: ")
    )

  (defun jg_org/open_link_in_buffer ()
    """ a util function to force links to be open in emacs  """
    (interactive)
    (org-open-at-point 'in-emacs)
    )

  (defun jg_org/open_link_externally ()
    """ Open a link, forcing it to be external to emacs """
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively 'org-open-at-point)))

  (defun jg_org/quicklook-link ()
    (let* ((context (org-element-lineage
                     (org-element-context)
                     '(clock comment comment-block footnote-definition
                             footnote-reference headline inline-src-block inlinetask
                             keyword link node-property planning src-block timestamp)
                     t))
           (type (org-element-property :type context))
           (path (org-element-property :path context)))
      (if (equal type "file")
          (call-process "qlmanage" nil 0 nil "-x" path)
        (message "Link not a file"))))

  (defun jg_org/change_link_name (name)
    """ Change the name of a link """
    (interactive "s")
    (let ((re org-bracket-link-regexp))
      (save-excursion
        (beginning-of-line)
        (search-forward-regexp re (line-end-position))
        (replace-match name nil nil nil 3)
        )
      )
    )

  (defun jg_org/list-agenda-files ()
    """ Creates a temporary, Org-mode buffer with links to agenda files """
    (interactive)
    (with-output-to-temp-buffer "*Agenda Files*"
      (set-buffer "*Agenda Files*")
      (insert "Agenda Files: ")
      (insert "\n")
      (mapc (lambda (x)
              (let ((file_name (last (split-string x "/" t ".org"))))
                (insert (format "[[%s][%s]]\n" x file_name))
                )) org-agenda-files)
      (org-mode)
      )
    )

  )

(when (configuration-layer/package-usedp 'org-ref)
  (defun jg_org/bibtex-load-random ()
  """ Run in a bibtex file, opens a random entry externally,
      and logs it has been opened in a separate file """
  (interactive)
  (widen)
  (let* ((location (f-dirname (buffer-file-name)))
         (log_file (f-join location ".emacs_rand_bib_log"))
         (log_hash (if (f-exists? log_file) (with-temp-buffer
                                              (insert-file-contents log_file)
                                              (let ((uf (make-hash-table :test 'equal)))
                                                (seq-each (lambda (x) (puthash x 't uf)) (split-string (buffer-string) "\n"))
                                                uf))
                     (make-hash-table :test 'equal)))
         )
    ;; go to random line
    (goto-char (random (point-max)))
    (org-ref-bibtex-next-entry)
    (let ((entry (bibtex-parse-entry)))
      (while entry
        (if (gethash (alist-get "=key=" entry nil nil 'equal) log_hash)
            (progn (goto-char (random (point-max)))
                   (org-reg-bibtex-next-entry)
                   (setq entry (bibtex-parse-entry)))
          (progn
            (write-region (alist-get "=key=" entry nil nil 'equal)
                          nil log_file 'append)
            (write-region "\n" nil log_file 'append)
            (bibtex-narrow-to-entry)
            (goto-char (point-min))
            (org-open-link-from-string (message "[[%s]]" (bibtex-text-in-field "file")))
            (setq entry nil)
            )
          )
        )
      )
    )
  )
  (defun jg_org/org-ref-open-bibtex-pdf ()
  "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((bibtex-expand-strings t)
           (entry (bibtex-parse-entry t))
           (key (reftex-get-bib-field "file" entry))
           (pdf (string-join `("/" ,(car (split-string key ":" 't))))))
      (message pdf)
      (if (file-exists-p pdf)
          (org-open-link-from-string (format "[[file:%s]]" pdf))
        (ding)))))
)


(when (configuration-layer/package-usedp 'org-pomodoro)
  (defun jg_org/pomodoro-start-hook ()
  ;; tweet out start and end points
  ;; use org-pomodoro-end-time
  (jg_twitter/twitter-tweet-text
   (format "Emacs Pomodoro Timer Session to end: %s"
           (format-time-string "%H:%M (%d, %b, %Y)" org-pomodoro-end-time)))
  )
  (defun jg_org/pomodoro-end-hook ()
  ;; create the temp buffer
  (progn
    (evil-window-new (get-buffer-window (current-buffer))
                     jg_org/pomodoro-buffer-name)
    (set (make-local-variable 'backup-inhibited) t)
    (auto-save-mode -1)
    (evil-window-set-height 10)
    (evil-initialize-local-keymaps)
    (evil-local-set-key 'normal (kbd "C-c C-c")
                        'jg_org/pomodoro-finish)
    (insert jg_org/pomodoro-log-message)
    (insert "Pomodoro Session: ")
    (redraw-display)
    )
  )
  (defun jg_org/pomodoro-finish ()
  ;; get the text
  (interactive)
  (let* ((text (buffer-substring (length jg_org/pomodoro-log-message) (point-max)))
         (time (format-time-string "(%Y/%b/%d) %H:%M" (current-time)))
         (formatted (format "** %s\n    %s\n" time (string-trim text)))
         )
    ;; tweet it
    (jg_twitter/twitter-tweet-text text nil '(jg_twitter/tweet_sentinel))
    ;;add it to the pomodoro log file
    (append-to-file formatted nil (expand-file-name jg_org/pomodoro-log-file))
    )
  )
)
