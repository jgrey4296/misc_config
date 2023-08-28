;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 17, 2023
;; Modified: March 17, 2023
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

(defer-load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")

(use-package! switch-window
  :when (modulep! +switch-window)
  :defer t
  :init
  (global-set-key [remap other-window] #'switch-window)
  :config
  (setq switch-window-shortcut-style 'qwerty)
  )

(use-package! ace-window
  :unless (modulep! +switch-window)
  :defer t
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (unless (modulep! +numbers)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
  (setq aw-scope 'frame
        aw-background t)
  )

(use-package! winum
  :when (modulep! +numbers)
  :after-call doom-switch-window-hook
  :config
  ;; winum modifies `mode-line-format' in a destructive manner. I'd rather leave
  ;; it to modeline plugins (or the user) to add this if they want it.
  (setq winum-auto-setup-mode-line nil)
  (winum-mode +1)
  (map! :map evil-window-map
        "0" #'winum-select-window-0-or-10
        "1" #'winum-select-window-1
        "2" #'winum-select-window-2
        "3" #'winum-select-window-3
        "4" #'winum-select-window-4
        "5" #'winum-select-window-5
        "6" #'winum-select-window-6
        "7" #'winum-select-window-7
        "8" #'winum-select-window-8
        "9" #'winum-select-window-9))

;;; config.el ends here
