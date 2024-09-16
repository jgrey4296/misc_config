;;; +extra-configs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! toc-org ; auto-table of contents
  :commands toc-org-enable
  :hook (org-mode . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh")
  )

(use-package! org-crypt ; built-in
  :commands org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry
  :hook (org-fold-reveal-start-hook . org-decrypt-entry)
  :preface
  (after! org
    (add-to-list 'org-tags-exclude-from-inheritance "crypt")
    (add-hook! 'org-mode-hook
      (add-hook 'before-save-hook 'org-encrypt-entries nil t)
      )
    )
  )

(use-package! org-unit-test
  :when (modulep! +unittest)
  :commands org-unit-test-minor-mode
  )

(use-package! org-yt)

(use-package! orgit)

(use-package! evil-org-agenda
  :when (modulep! :editor evil)
  :disabled t
  :hook (org-agenda-mode . evil-org-agenda-mode)
  :config
  (evil-org-agenda-set-keys)
  (evil-define-key* 'motion evil-org-agenda-mode-map
    (kbd doom-leader-key) nil)

  )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 10, 2024
;; Modified:   September 10, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +extra-configs.el ends here
