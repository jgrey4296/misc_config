;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: August 20, 2023
;; Modified: August 20, 2023
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

(use-package! centered-cursor-mode
  :commands centered-cursor-mode
  :init
  (defvar centered-cursor-mode nil)
  )

(use-package! better-jumper
  :hook (doom-first-input . better-jumper-mode)
  :commands doom-set-jump-a doom-set-jump-maybe-a doom-set-jump-h
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)
  :config

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  ;;
  ;; I'm not advising `kill-buffer' because I only want this to affect
  ;; interactively killed buffers.
  (advice-add #'kill-current-buffer :around #'doom-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'doom-set-jump-a))

(use-package! imenu
  :defer t
  :config
  (add-hook 'imenu-after-jump-hook #'recenter)
  )

(use-package! paren-state)

(use-package! avy
  :init
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        ;; the unpredictability of this (when enabled) makes it a poor default
        avy-single-candidate-jump nil
        )
  )

;;; config.el ends here
