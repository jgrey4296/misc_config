;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: January 26, 2023
;; Modified: January 26, 2023
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

(use-package! conf-mode
  :commands conf-mode conf-toml-mode
  :config
  ;; Derived modes: conf-unix, conf-windows, conf-javaprop
  ;; conf-space, conf-colon, conf-ppd, conf-xdefaults,
  ;; conf-toml, conf-desktop


  (add-hook! 'conf-mode-hook :depth 99
             #'librarian-insert-minor-mode
             )

  (add-hook! 'conf-toml-mode-hook :depth 99
             #'abbrev-mode
             #'outline-minor-mode
             )

  (setq-hook! 'conf-toml-mode-hook
    outline-regexp "\[\[?[a-zA-Z0-9\.]+\]?\]"
    outline-heading-end-regexp "\n"
    )

  )

(use-package! toml-mode
  :commands toml-mode
  :config
  ;; toml mode is derived from conf-mode

  (add-hook! 'toml-mode-hook :depth 99
             #'abbrev-mode
             #'outline-minor-mode
             )

  (setq-hook! 'toml-mode-hook
    outline-regexp "\[\[?[a-zA-Z0-9\.]+\]?\]"
    outline-heading-end-regexp "\n"
    )
  )

(use-package! toml-ts-mode
  :commands toml-ts-mode
  :config
  ;; derived from text mode

  (add-hook! 'toml-ts-mode-hook :depth 99
             #'abbrev-mode
             #'outline-minor-mode
             #'librarian-insert-minor-mode
             )

  (setq-hook! 'toml-ts-mode-hook
    outline-regexp "\[\[?[a-zA-Z0-9\.]+\]?\]"
    outline-heading-end-regexp "\n"
    )
  )

;;
;;; config.el ends here
