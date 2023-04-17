;;; +vars.el -*- lexical-binding: t; -*-

(after! company-dict
  (setq company-dict-dir (expand-file-name "templates/company-dicts" doom-user-dir)

        )
  (add-to-list 'company-backends 'company-dict)
  )
