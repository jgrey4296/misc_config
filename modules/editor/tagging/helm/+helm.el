;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;-- utils
(defun +jg-tag-save-helm-buffer ()
  (interactive)
  (let ((results (with-helm-buffer (buffer-string))))
    (helm-exit-and-execute-action
     #'(lambda (x)
         (with-temp-buffer-window "TestBuffer" 'display-buffer-pop-up-frame nil
           (princ results)
           )
         )
     )
    )
  )
;;-- end utils

;;-- dual helm/action
(defun +jg-tag-file-select-helm (candidates)
    " Given a list of Files, provide a helm to open them "
    (interactive)
    ;;(message "File Select Helm Candidates: %s" (helm-marked-candidates))
    ;;process candidates?
    (let*(;;(candidate-names (mapcar 'car (helm-marked-candidates)))
          (candidate-values (helm-marked-candidates))
          (all-candidates (-flatten (mapcar (lambda (x) (plist-get x :files)) candidate-values)))
          (source (cons `(candidates . ,all-candidates) jg-tag-file-select-source)))
      (helm :sources source
            :full-frame t
            :buffer "*helm file select*"
            )
      )
    )
(defun +jg-tag-file-display (candidates)
  (interactive)
  (let*((candidates (plist-get (car (helm-marked-candidates)) :files)))
    (with-temp-buffer-window "Helm Twitter Grep Results"
        'display-buffer-pop-up-window nil
      (mapcar (lambda (x) (princ x) (princ "\n")) candidates)
      )
    )
  )
;;-- end dual helm/action

;;-- helms
(defun +jg-tag-helm-tag-twitter ()
    "Run a Helm for searching twitter tags"
    (interactive)
    (helm :sources jg-tag-twitter-tag-helm-source
          :full-frame t
          :buffer "*helm twitter*"
          :truncate-lines t
          )
    )
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
(defun +jg-tag-helm-account-twitter ()
    "Run a Helm for searching twitter users"
    (interactive)
    (helm :sources jg-tag-twitter-account-helm-source
          :full-frame t
          :buffer "*helm twitter heading*"
          :truncate-lines t
          )
    )
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
;;-- end helms
