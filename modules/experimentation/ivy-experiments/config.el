;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: July 18, 2023
;; Modified: July 18, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(defer-load! "+vars")
;; (defer-load! "+spec-defs")
(defer-load! jg-bindings-total "+bindings")

(use-package! ivy-prescient
  :when (modulep! +prescient)
  :hook (ivy-mode . ivy-prescient-mode)
  :hook (ivy-prescient-mode . prescient-persist-mode)
  :commands +ivy-prescient-non-fuzzy
  :init
  (setq prescient-filter-method
        (if (modulep! +fuzzy)
            '(literal regexp initialism fuzzy)
          '(literal regexp initialism)))
  :config
  ;; REVIEW Remove when radian-software/prescient.el#102 is resolved
  (add-to-list 'ivy-sort-functions-alist '(ivy-resume))
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer lsp-ivy-workspace-symbol
          ivy-resume ivy--restore-session counsel-grep counsel-git-grep
          counsel-rg counsel-ag counsel-ack counsel-fzf counsel-pt counsel-imenu
          counsel-yank-pop counsel-recentf counsel-buffer-or-recentf
          counsel-outline counsel-org-goto counsel-jq)
        ivy-prescient-retain-classic-highlighting t)
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str))
    )

  ;; NOTE prescient config duplicated with `company'
  (setq prescient-save-file (concat doom-cache-dir "prescient-save.el"))
  )
;;; config.el ends here
