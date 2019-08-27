(defconst helm-org-packages
  '(
    helm-org
    )
)

(defun helm-org/init-helm-org ()
  (use-package helm-org
    :defer t
    :init
    (progn
      (evil-define-key 'normal org-mode-map
        (kbd "g h")     'helm-org-in-buffer-headings
	      )
      )
    :config
    (progn
      (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
      (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
      )
    )
  )

