;;; +flycheck.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! flycheck
  :commands (flycheck-list-errors flycheck-buffer flycheck-mode global-flycheck-mode)
  ;; :hook (doom-first-buffer . global-flycheck-mode)
  :init
  (setq flycheck-global-modes nil)

  :config
  (after! fringe
    ;; Let diff-hl have left fringe, flycheck can have right fringe
    ;; A non-descript, left-pointing arrow
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow [16 48 112 240 112 48 16] nil nil 'center)
    )

  (delq 'new-line flycheck-check-syntax-automatically)
  (remove-hook 'after-change-major-mode-hook #'global-flycheck-mode-enable-in-buffers)

)

(use-package! flycheck-popup-tip
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :config
  (after! evil
    ;; Don't display popups while in insert or replace mode, as it can affect
    ;; the cursor's position or cause disruptive input delays.
    (add-hook! '(evil-insert-state-entry-hook evil-replace-state-entry-hook)
               #'flycheck-popup-tip-delete-popup)
    (advice-add 'flycheck-popup-tip-show-popup :before-while #'+syntax--disable-flycheck-popup-tip-maybe-a)
    )
  )

(spec-handling-new! flycheck nil :loop 'hook
                    :struct '(:head checker :rest rest)
                    :doc ""
                    (flycheck-mode)
                    (let ((head (plist-get val :head)))
                      (flycheck-select-checker head)
                      (dolist (next (plist-get val :rest))
                        (flycheck-add-next-checker head next 'append)
                        )
                      )
                    )

(defvar flycheck-checkers)

(defvar flycheck-disabled-checkers)

(spec-handling-setq! flycheck 50
                     flycheck-display-errors-delay 1
                     flycheck-display-errors-function nil
                     flycheck-help-echo-function nil
                     flycheck-process-error-functions nil
                     flycheck-check-syntax-automatically '(save idle-change mode-enabled)
                     flycheck-idle-change-delay 1.0
                     flycheck-buffer-switch-check-intermediate-buffers t
                     flycheck-display-errors-delay 0.25
                     flycheck-popup-tip-error-prefix "X "
                     flycheck-posframe-warning-prefix "! "
                     flycheck-posframe-info-prefix "··· "
                     flycheck-posframe-error-prefix "X "
                     flycheck-indication-mode 'right-fringe
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
;;; +flycheck.el ends here
