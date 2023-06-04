;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 07, 2023
;; Modified: April 07, 2023
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

(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package! flycheck
  :commands flycheck-list-errors flycheck-buffer
  ;; :hook (doom-first-buffer . global-flycheck-mode)
  :init
  (defvar flycheck-checkers)
  (defvar flycheck-disabled-checkers)

  (spec-handling-add! python-env
                      '(flycheck
                        (:support flycheck #'(lambda (path name)
                                               (unless flycheck-enabled-checkers
                                                 (let ((chosen (intern (ivy-read "Flychecker: " flycheck-disabled-checkers :require-match t))))
                                                   (delete chosen flycheck-disabled-checkers)
                                                   (add-to-list flycheck-enabled-checkers chosen)
                                                   ))
                                               (add-hook 'python-mode-hook #'flycheck-mode)
                                               )
                                  (-partial #'flycheck-mode -1)
                                  )
                        (:teardown flycheck (-partial flycheck-mode -1))
                        )
                      )
  (setq flycheck-global-modes nil)

  :config
  (add-hook! 'doom-escape-hook :append
    (defun +syntax-check-buffer-h ()
      "Flycheck buffer on ESC in normal mode."
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil)))

  (remove-hook! 'after-change-major-mode-hook
    #'global-flycheck-mode-enable-in-buffers
    )
)

(use-package! flycheck-popup-tip
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (after! evil
    ;; Don't display popups while in insert or replace mode, as it can affect
    ;; the cursor's position or cause disruptive input delays.
    (add-hook! '(evil-insert-state-entry-hook evil-replace-state-entry-hook)
               #'flycheck-popup-tip-delete-popup)
    (defadvice! +syntax--disable-flycheck-popup-tip-maybe-a (&rest _)
      :before-while #'flycheck-popup-tip-show-popup
      (if evil-local-mode
          (eq evil-state 'normal)
        (not (bound-and-true-p company-backend)))))
  )

(use-package! flycheck-posframe
  :when (modulep! +childframe)
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (after! company
    ;; Don't display popups if company is open
    (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p))
  (after! evil
    ;; Don't display popups while in insert or replace mode, as it can affect
    ;; the cursor's position or cause disruptive input delays.
    (add-hook! 'flycheck-posframe-inhibit-functions
               #'evil-insert-state-p
               #'evil-replace-state-p))
  )

;;; config.el ends here
