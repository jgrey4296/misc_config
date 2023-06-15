;;; browse.el -*- lexical-binding: t; -*-

(defvar browse-select-default-prog "firefox")

(defvar browse-select-variants ())

(defvar browse-select-variant-file "~/.browsers")

(defvar browse-select-use-preview t)

(defvar browse-select-pdf-args  '("-a" "Preview" "-nF"))

(defvar browse-select-epub-args '("-a" "ebook-viewer"))

(defvar browse-select-curl-cmd  "curl")

(defvar browse-select-curl-args  '("-sLI"))

(defvar browse-select-focus-target  "iTerm")

(defvar browse-select-twitter-url "https://twitter.com/search?q=%s")

(defvar browse-select-amazon-url "")


;;;###autoload
(defun browse-select-regain-focus ()
  " For when a command will change focus to something else (preview, firefox)
force it back to the terminal
"
  (when (and (boundp 'IS-MAC) IS-MAC)
    (call-process "osascript" nil nil nil
                  "-e" (format "tell application \"%s\"" browse-select-focus-target)
                  "-e" "activate"
                  "-e" "end tell"
                  )
    )
  )

;;;###autoload
(defun browse-select-load-variants ()
  " Get a list of possible browsers to use from persistent file"
  (with-temp-buffer
    (insert-file (expand-file-name browse-select-variant-file))
    (setq browse-select-variants (split-string (buffer-string) "\n" t " +"))
    )
  )

;;;###autoload
(defun browse-select-goto-url (&optional url)
  " quick access to search handlers "
  (interactive)
  (let ((url (cond (url url)
                   ((eq evil-state 'visual)
                    (buffer-substring-no-properties evil-visual-beginning evil-visual-end))
                   (t nil)))
        )
    (cond ((not url)
           (+lookup/online-select)) ;; TODO factor +lookup into browse-select
          ((f-exists? url)
           (shell-command (format "open %s" url)))
          (t
           (call-interactively #'+lookup/online url) ;;TODO
           )
          )
    )
  )

;;;###autoload
(defun browse-select-toggle-browsing ()
  (interactive)
  (let* ((index (1+ (or (-elem-index browse-select-default-prog browse-select-variants) 0)))
        (new-prog (nth (mod index (length browse-select-variants)) browse-select-variants))
        )
    (message "Browsing: %s" (setq browse-select-default-prog new-prog))
    )
  )

;;;###autoload
(defun browse-select-toggle-preview ()
  (interactive)
  (message "Using Preview for pdfs: %s"
           (setq browse-select-use-preview (not browse-select-use-preview)))
  )

;;;###autoload
(defun browse-select-default (url &rest args)
  " Find and call the appropriate browser program,
after `browse-url-handlers` have processed the url
"
  (cond ((-contains? args 'quicklook)
         (start-process "open-ql" "*browse-select*" "qlmanage" "-p" (shell-quote-argument url)))
        ((and (-contains? args 'local) (f-ext? url "epub"))
         (apply 'start-process "open-epub" "*browse-select*" "open" url browse-select-epub-args)
         )
        ((and (-contains? args 'local) (f-ext? url "pdf") browse-select-use-preview)
         (apply 'start-process "open-pdf" "*browse-select*" "open" url browse-select-pdf-args)
         )
        ((not (s-equals? browse-select-default-prog "eww"))
         (message "Using %s" browse-select-default-prog)
         (start-process "open-url" "*browse-select*" browse-select-default-prog url)
         )
        (t
         (eww-browse-url url args))
        )

  (sleep-for 2)
  (browse-select-regain-focus)
  )

;;;###autoload
(defun browse-select-twitter (url &rest args)
  (browse-select-default (format browse-select-twitter-url (substring url 1)))
  )

;;;###autoload
(defun browse-select-amazon (url &rest args)
  ;; TODO Handle US and UK
  (signal 'browse-todo url)
  )

(provide 'browse-select)
