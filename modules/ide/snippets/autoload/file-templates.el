;;; editor/file-templates/autoload.el -*- lexical-binding: t; -*-

;; Use file-template specs

(defun +file-templates-in-emacs-dirs-p (file)
  "Returns t if FILE is in Doom or your private directory."
  (or (file-in-directory-p file doom-user-dir)
      (file-in-directory-p file doom-emacs-dir)))

(defun +file-template-p (rule)
  "Return t if RULE applies to the current buffer."
  (let ((pred (car rule))
        (plist (cdr rule)))
    (and (or (and (symbolp pred) (eq major-mode pred))
             (and (stringp pred) (stringp buffer-file-name) (string-match-p pred buffer-file-name)))
         (or (not (plist-member plist :when)) (funcall (plist-get plist :when) buffer-file-name))
         rule)))

;;;###autoload
(defun +file-templates-maybe-expand-h ()
  "Check if the current buffer is a candidate for file template expansion. It
must be non-read-only, empty, and there must be a rule in
`+file-templates-alist' that applies to it."
  (interactive)
  (and (not +file-templates-inhibit)
       buffer-file-name        ; this buffer represents a file and
       (not buffer-read-only)  ; ...isn't read-only
       (bobp) (eobp)           ; ...is empty
       (not (member (substring (buffer-name) 0 1) '("*" " ")))  ; ...isn't a "special" buffer
       (not (bound-and-true-p org-capture-current-plist))  ; ...isn't an org-capture buffer
       (not (file-exists-p buffer-file-name))  ; ...is a new file
       (not (buffer-modified-p))    ; ...hasn't been modified
       (null (buffer-base-buffer))  ; ...isn't an indirect clone
       (when-let (rule (cl-find-if #'+file-template-p +file-templates-alist))
         (apply #'+file-templates--expand rule))))

;;;###autoload
(cl-defun +file-templates--expand (pred &key project mode trigger ignore _when &allow-other-keys)
  "Auto insert a yasnippet snippet into current file and enter insert mode (if
evil is loaded and enabled)."
  (when (and pred (not ignore))
    (when (if project (doom-project-p) t)
      (unless mode (setq mode (if (and (symbolp pred) (not (booleanp pred))) pred major-mode)))
      (unless mode (user-error "Couldn't determine mode for %s file template" pred))
      (unless trigger (setq trigger +file-templates-default-trigger))
      (when (functionp trigger) (funcall trigger))
      (unless yas-minor-mode (require 'yasnippet) (yas-minor-mode-on))
      (when-let ((ymm yas-minor-mode)
                 (template (cl-find trigger (yas--all-templates (yas--get-snippet-tables mode)) :key #'yas--template-key :test #'equal)))
        (yas-expand-snippet (yas--template-content template)))
      (when (and (featurep 'evil) evil-local-mode)
        (evil-initialize-state 'insert))))
  )

;;;###autoload
(defun +file-templates-get-short-path ()
  "Fetches a short file path for the header in Doom module templates."
  (let ((path (file-truename (or buffer-file-name default-directory))))
    (save-match-data
      (cond ((string-match "/modules/\\(.+\\)$" path)
             (match-string 1 path))
            ((file-in-directory-p path doom-emacs-dir)
             (file-relative-name path doom-emacs-dir))
            ((file-in-directory-p path doom-user-dir)
             (file-relative-name path doom-user-dir))
            ((abbreviate-file-name path))))))

;;;###autoload
(defun +file-templates-module-for-path (&optional path)
  "Generate a title for a doom module's readme at PATH."
  (let ((m (doom-module-from-path (or path (buffer-file-name)))))
    (if (eq (cdr m) 'README.org)
        (symbol-name (car m))
      (format "%s %s" (car m) (cdr m)))))

;;; Commands
;;;###autoload
(defun +file-templates/insert-license ()
  "Insert a license file template into the current file."
  (interactive)
  (require 'yasnippet)
  (unless (gethash 'text-mode yas--tables)
    (yas-reload-all t))
  (let ((templates
         (let (yas-choose-tables-first ; avoid prompts
               yas-choose-keys-first)
           (cl-loop for tpl in (yas--all-templates (yas--get-snippet-tables 'text-mode))
                    for uuid = (yas--template-uuid tpl)
                    if (string-prefix-p "__license-" uuid)
                    collect (cons (string-remove-prefix "__license-" uuid) tpl)))))
    (when-let (uuid (yas-choose-value (mapcar #'car templates)))
      (yas-expand-snippet (cdr (assoc uuid templates))))))

;;;###autoload
(defun +file-templates/debug ()
  "Tests the current buffer and outputs the file template rule most appropriate
for it. This is used for testing."
  (interactive)
  (cl-destructuring-bind (pred &rest plist &key trigger mode &allow-other-keys)
      (or (cl-find-if #'+file-template-p +file-templates-alist)
          (user-error "Found no file template for this file"))
    (if (or (functionp trigger)
            (cl-find trigger
                     (yas--all-templates
                      (yas--get-snippet-tables
                       mode))
                     :key #'yas--template-key :test #'equal))
        (message "Found %s" (cons pred plist))
      (message "Found rule, but can't find associated snippet: %s" (cons pred plist)))))

;;; Trigger functions
(defun +file-templates-insert-doom-docs-fn ()
  "Expand one of Doom's README templates depending."
  (+file-templates--expand
   t :trigger
   (let ((path (file-truename (buffer-file-name))))
     (cond ((string-match-p "/modules/[^/]+/README\\.org$" path)
            "__doom-category-readme")
           ((string-match-p "/modules/[^/]+/[^/]+/README\\.org$" path)
            "__doom-readme")
           ((file-in-directory-p path doom-docs-dir)
            "__doom-docs")
           ("__")))))
