;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(local-load! "sources")

;;;###autoload
(defun +jg-tag-helm-tag-twitter ()
    "Run a Helm for searching twitter tags"
    (interactive)
    (helm :sources jg-tag-twitter-tag-helm-source
          :full-frame t
          :buffer "*helm twitter*"
          :truncate-lines t
          )
    )

;;;###autoload
(defun +jg-tag-helm-twitter-grep (arg)
  (interactive "p")
    (helm-set-local-variable
     'helm-grep-include-files (format "--include=%s" jg-tag-loc-twitter-grep-index)
     'helm-grep-last-targets `(,jg-tag-loc-twitter-grep-index)
     'default-directory jg-tag-loc-default-helm-directory
     )
    (helm :sources (if (eq arg 4)
                       jg-tag-twitter-grep-helm-source-alt
                     jg-tag-twitter-grep-helm-source)
          :full-frame t
          :buffer "*helm grep twitter*"
          :truncate-lines t
          )
    )

;;;###autoload
(defun +jg-tag-helm-account-twitter ()
    "Run a Helm for searching twitter users"
    (interactive)
    (helm :sources jg-tag-twitter-account-helm-source
          :full-frame t
          :buffer "*helm twitter heading*"
          :truncate-lines t
          )
    )

;;;###autoload
(defun +jg-tag-helm-unified ()
    (interactive)
    ;;Load headings if necessary
    (if (null jg-tag-twitter-account-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-twitter-account-helm-candidates '())
          (insert-file jg-tag-loc-twitter-account-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-twitter-account-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;Load twitter users if necessary
    (if (null jg-tag-twitter-tag-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-twitter-tag-helm-candidates '())
          (insert-file jg-tag-loc-twitter-tag-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-twitter-tag-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;Set local variables for bookmarks
    (helm-set-local-variable
     'helm-grep-include-files (format "--include=%s" jg-tag-loc-bookmarks)
     'helm-grep-last-targets `(,jg-tag-loc-bookmarks)
     'default-directory jg-tag-loc-default-helm-directory
     )
    ;;add candidates to source
    (let* ((source-tw (cons `(candidates . jg-tag-twitter-tag-helm-candidates) jg-tag-twitter-tag-helm-source))
           (source-heading (cons `(candidates . jg-tag-twitter-account-helm-candidates) jg-tag-twitter-account-helm-source)))
      ;;call helm
      (helm :sources (list source-heading jg-tag-bookmark-helm-source)
            :full-frame t
            :buffer "*Helm unified*"
            :truncate-lines t
            )
      )
    )
