(defconst doctest-packages
  '(
    org
    parsec
    )
  )

(defun doctest/init-parsec ()
  (use-package parsec
    :commands (parsec-with-input)
    :defer t)
  )

(defun doctest/post-init-org ()
  (defun doctest/org-mod-map ()
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      ". T" 'doctest/test-org-file)
    )
  (add-hook 'org-mode-hook 'doctest/org-mod-map)
  )
