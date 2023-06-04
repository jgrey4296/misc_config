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

;;;###autoload (autoload 'ibuffer-filter-by-workspace-buffers "modules/editor/workspaces/autoload/ibuffer" nil t)
(define-ibuffer-filter workspace-buffers
    "Filter for workspace buffers"
  (:reader (ivy-read "workspace name: " (+workspace-list-names))
   :description "workspace")
  (memq buf (+workspace-buffer-list (+workspace-get qualifier)))
  )

;;;###autoload (autoload 'ibuffer-filter-by-window-ring-buffers "modules/editor/workspaces/autoload/ibuffer" nil t)
(define-ibuffer-filter window-ring
    "Filter by current window ring"
  (:description "Window-Ring")
  (window-ring-buffer-p nil buf)
)


;;;###autoload (autoload 'ibuffer-make-column-workspace "modules/editor/workspaces/autoload/ibuffer.el" nil t)
(define-ibuffer-column workspace
  (:name "Workspace"
   :inline t
   )
  (string-join (cl-loop for workspace in (+workspace-list-names)
           if (persp-contain-buffer-p (current-buffer) (+workspace-get workspace))
           collect workspace
           ) ", ")
  )

(defun +jg-ibuffer-generate-project-groups ()
  "Create a set of ibuffer filter groups based on the projectile root dirs of buffers."
  (let ((roots (ibuffer-remove-duplicates
                (delq nil (mapcar 'ibuffer-projectile-root (buffer-list))))))
    (mapcar (lambda (root)
              (cons (funcall ibuffer-projectile-group-name-function (car root) (cdr root))
                    `((projectile-root . ,root))))
            roots))
  )

(defun +jg-ibuffer-generate-workspace-groups ()
  (mapcar (lambda (workspace)
            (list (format "Workspace: %s" workspace) `(workspace-buffers . ,workspace)))
          (+workspace-list-names))
  )
