;;; +flycheck.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(unless (featurep 'flycheck)
  (defvar flycheck-checkers)
  (defvar flycheck-disabled-checkers)
  )

(after! jg-evil-ex-bindings
  (evil-ex-define-cmd "er[rors]"    #'flycheck-list-errors)
  )

(use-package! flycheck
  :init
  (setq flycheck-global-modes nil)

  :config
  (after! fringe
    ;; Let diff-hl have left fringe, flycheck can have right fringe
    ;; A non-descript, left-pointing arrow
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow [16 48 112 240 112 48 16] nil nil 'center)
    )

  (remove-hook 'after-change-major-mode-hook #'global-flycheck-mode-enable-in-buffers)
  (advice-add 'flycheck-error-list-apply-filter :filter-return #'jg-flycheck-error-list-apply-filter-a)
  (advice-add 'flycheck-list-errors :after #'jg-flycheck-set-filter-to-highest-actual-a)

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

(speckler-new-hook! flycheck (key val)
  "sets up a stack of checkers"
  :struct '(:head checker :rest rest)
  (let ((head (plist-get val :head)))
    (flycheck-select-checker head)
    (dolist (next (plist-get val :rest))
      (flycheck-add-next-checker head next 'append)
      )
    )
  )

(speckler-setq! flycheck ()
  flycheck-display-errors-delay 1
  flycheck-display-errors-function 'flycheck-display-error-messages
  flycheck-help-echo-function nil
  flycheck-process-error-functions '(flycheck-add-overlay)
  flycheck-check-syntax-automatically '(save idle-change mode-enabled)
  flycheck-idle-change-delay 1.0
  flycheck-buffer-switch-check-intermediate-buffers t
  flycheck-display-errors-delay 0.25
  flycheck-popup-tip-error-prefix "X "
  flycheck-indication-mode 'right-fringe
  )

(speckler-add! lib-env ()
  :override nil
  `(flycheck
    :setup    #'(lambda (state &rest rest) (require 'flycheck))
    :start    #'(lambda (state &rest rest) (add-hook 'prog-mode-hook #'flycheck-mode))
    :stop     #'(lambda (state &rest rest) (remove-hook 'prog-mode-hook #'flycheck-mode))
    :teardown #'(lambda (state &rest rest) nil)
    :modeline #'(lambda (state &rest rest) "chk")
    :headline #'(lambda (state &rest rest) '(:eval (doom-modeline-segment--check)))
    )
  )

(speckler-add! popup ()
  '(flycheck
    ("^\\*Flycheck error messages\\*" :select nil)
    ("^\\*Flycheck errors\\*" :size 0.25)
    )
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
