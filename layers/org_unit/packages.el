(defconst org_unit-packages
  '(
    org
    parsec
    )
  )

(defun org_unit/init-parsec ()
  (use-package parsec
    :commands (parsec-with-input)
    :defer t)
  )

(defun org_unit/post-init-org ()
  (defun org_unit/org-mod-map ()
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      ". T" 'org_unit/test-org-file)
    )
  (add-hook 'org-mode-hook 'org_unit/org-mod-map)
  )
