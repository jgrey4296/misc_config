;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: May 28, 2023
;; Modified: May 28, 2023
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


(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")
;;todo https://github.com/Bitnut/diffgit

(use-package! diff-hl
  :hook (find-file    . diff-hl-mode)
  :hook (vc-dir-mode  . diff-hl-dir-mode)
  :hook (dired-mode   . diff-hl-dired-mode)
  ;; :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :commands diff-hl-stage-current-hunk diff-hl-revert-hunk diff-hl-next-hunk diff-hl-previous-hunk
  :config
  ;; PERF: reduce load on remote
  (defvaralias 'diff-hl-disable-on-remote '+vc-gutter-in-remote-files)

  ;; UX: Refresh git-gutter on ESC or refocusing the Emacs frame.
  (add-hook! '(doom-escape-hook doom-switch-window-hook) :append #'+vc-gutter-update-h)
  ;; UX: Update diff-hl when magit alters git state.
  (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (add-hook! 'diff-hl-flydiff-mode-hook #'+vc-gutter-init-flydiff-mode-h)
  (add-hook 'diff-hl-mode-hook #'+vc-gutter-fix-diff-hl-faces-h)
  )

(use-package! diff-mode :defer t)
(use-package! ediff
  :defer t
  :config
  (add-hook! 'ediff-before-setup-hook
    (defun doom-ediff-save-wconf-h ()
      (setq doom--ediff-saved-wconf (current-window-configuration))))
  (add-hook! '(ediff-quit-hook ediff-suspend-hook) :append
    (defun doom-ediff-restore-wconf-h ()
      (when (window-configuration-p doom--ediff-saved-wconf)
        (set-window-configuration doom--ediff-saved-wconf))))
  )
(use-package! vdiff :defer t)

;;; config.el ends here
