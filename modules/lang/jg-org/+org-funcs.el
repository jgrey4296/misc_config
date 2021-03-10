;;; org/jg-org/+org-funcs.el -*- lexical-binding: t; -*-


(defun +jg-org-insert-heading-trio ()
  (interactive)
  (org-insert-subheading 1)
  (insert "1: ")
  (org-insert-heading 3 nil nil)
  (insert "2: ")
  (org-insert-heading 1 nil nil)
  (insert "3: ")
  )
(defun +jg-org-open_link_in_buffer ()
  """ a util function to force links to be open in emacs  """
  (interactive)
  (org-open-at-point 'in-emacs)
  )
(defun +jg-org-open_link_externally ()
  """ Open a link, forcing it to be external to emacs """
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively 'org-open-at-point)))
(defun +jg-org-link-not-exists-p ()
  (let* ((context (org-element-lineage
                   (org-element-context)
                   '(clock comment comment-block footnote-definition
                           footnote-reference headline inline-src-block inlinetask
                           keyword link node-property planning src-block timestamp)
                   t))
         (type (org-element-property :type context))
         (path (org-element-property :path context)))
    (and (equal type "file") (not (file-exists-p path)))))

(defun +jg-org-quicklook-link ()
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
(defun +jg-org-change_link_name (name)
  """ Change the name of a link """
  (interactive "s")
  (let ((re org-link-bracket-re))
    (save-excursion
      (beginning-of-line)
      (search-forward-regexp re (line-end-position))
      (replace-match name nil nil nil 2)
      )
    )
  )
(defun +jg-org-list-agenda-files ()
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

(defun +jg-org-fix-properties-drawers ()
  (interactive)
  (goto-char (point-min))
  ;; Find properties drawer
  (while (re-search-forward "^\s*:PROPERTIES:$" nil t)
    ;; for each :TERM:, ensure it is on its own line
    (let* ((context (org-element-context))
           (drawer-end (plist-get (cadr context) :contents-end))
           (drawer-type (car context))
           )
      (assert (or (equal 'property-drawer drawer-type) (equal 'drawer drawer-type)))
      (while (re-search-forward "\\(:[[:upper:]_]+:\\)" drawer-end t)
        (save-excursion
          (goto-char (match-beginning 0))
          (if (not (equal (point) (line-beginning-position)))
              (insert "\n"))
          )
        (if (looking-at-p "$")
            (join-line -1))
        )
      )
    )
  )
