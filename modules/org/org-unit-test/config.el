;;; specific/org-unit-test-layer/config.el -*- lexical-binding: t; -*-

(after! org
  (defun org-unit-test-layer/org-mod-map ()
    (map! :mode org-mode
          :localleader
      ". T" 'org-unit-test-layer/test-org-file)
    )
  (add-hook 'org-mode-hook 'org-unit-test-layer/org-mod-map)
  )
