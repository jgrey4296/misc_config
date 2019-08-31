(defconst helm-org-packages
  '(
    helm-org
    org
    )
)

(defun helm-org/init-helm-org ()
  (use-package helm-org
    :defer t
    :config
    (progn
      (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
      (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
      )
    )
  )

(defun helm-org/post-init-org ()
  (add-hook 'org-mode-hook (lambda ()
                             (evil-define-key 'normal org-mode-map
                               (kbd "g h")     'helm-org-in-buffer-headings
	                             )
                             (evil-define-key 'normal evil-org-mode-map
                               (kbd "g h")     'helm-org-in-buffer-headings
                             ))
  )
