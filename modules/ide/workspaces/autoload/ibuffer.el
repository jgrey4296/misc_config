;;; emacs/ibuffer/autoload/workspaces.el -*- lexical-binding: t; -*-
(require 'ibuf-ext)

;;;###autoload
(defun +ibuffer-workspace (workspace-name)
  "Open an ibuffer window for a workspace"
  (ibuffer nil (format "%s buffers" workspace-name)
           (list (cons 'workspace-buffers (+workspace-get workspace-name)))))

;;;###autoload
(defun +ibuffer/open-for-current-workspace ()
  "Open an ibuffer window for the current workspace"
  (interactive)
  (+ibuffer-workspace (+workspace-current-name)))

;;;###autoload
(defun +ibuffer/visit-workspace-buffer (&optional select-first)
  "Visit buffer, but switch to its workspace if it exists."
  (interactive "P")
  (let ((buf (ibuffer-current-buffer t)))
    (unless (buffer-live-p buf)
      (user-error "Not a valid or live buffer: %s" buf))
    (if-let (workspaces
             (cl-loop for wk in (+workspace-list)
                      if (+workspace-contains-buffer-p buf wk)
                      collect wk))
        (+workspace-switch
         (if (and (not select-first) (cdr workspaces))
             (or (completing-read "Select workspace: " (mapcar #'persp-name workspaces))
                 (user-error "Aborted"))
           (persp-name (car workspaces))))
      ;; Or add the buffer to the current workspace
      (persp-add-buffer buf))
    (switch-to-buffer buf)))

;;;###autoload (autoload 'ibuffer-filter-by-workspace-buffers "ide/workspaces/autoload/ibuffer" nil t)
(define-ibuffer-filter workspace-buffers
    "Filter for workspace buffers"
  (:reader (ivy-read "workspace name: " (+workspace-list-names))
   :description "workspace")
  (memq buf (+workspace-buffer-list (+workspace-get qualifier)))
  )

;;;###autoload (autoload 'ibuffer-filter-by-carousel-buffers "ide/workspaces/autoload/ibuffer" nil t)
(define-ibuffer-filter carousel
    "Filter by current window ring"
  (:description "Carousel")
  (carousel-buffer-p nil buf)
)


;;;###autoload (autoload 'ibuffer-make-column-workspace "ide/workspaces/autoload/ibuffer.el" nil t)
(define-ibuffer-column workspace
  (:name "Workspace")
  (string-join (cl-loop for workspace in (+workspace-list-names)
           if (persp-contain-buffer-p (current-buffer) (+workspace-get workspace))
           collect workspace
           ) ", ")
  )

(defun +jg-ibuffer-generate-workspace-groups ()
  (mapcar (lambda (workspace)
            (list (format "Workspace: %s" workspace) `(workspace-buffers . ,workspace)))
          (+workspace-list-names))
  )
