;;; +format.el -*- lexical-binding: t; -*-

;; Display buffer icons on GUI
(define-ibuffer-column icon (:name "  ")
  (let ((icon (if (and (buffer-file-name)
                       (all-the-icons-auto-mode-match?))
                  (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
    (if (symbolp icon)
        (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
      icon)))

;; Redefine size column to display human readable size
(define-ibuffer-column size
  (:name "Size"
   :inline t
   :header-mouse-map ibuffer-size-header-map)
  (file-size-human-readable (buffer-size)))


;; (when (modulep! :ui workspaces)
;;   (define-ibuffer-filter workspace-buffers
;;       "Filter for workspace buffers"
;;     (:reader (+workspace-get (read-string "workspace name: "))
;;      :description "workspace")
;;     (memq buf (+workspace-buffer-list qualifier)))

;;   (define-key ibuffer-mode-map [remap ibuffer-visit-buffer] #'+ibuffer/visit-workspace-buffer))
