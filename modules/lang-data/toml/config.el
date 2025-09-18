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
    ;; outline-regexp "\[\[?[a-zA-Z0-9\.]+\]?\]"
    outline-regexp (rx (| (: (1+ "[") (1+ (any "a-zA-Z0-9\.")) (1+ "]"))
                          (: "# " (1+ "-") line-end)
                          ))
    outline-heading-end-regexp "\n"
    )

  )

(use-package! toml-mode
  :commands toml-mode
  :config

  (add-hook! 'toml-mode-hook :depth -99
             #'abbrev-mode
             #'outline-minor-mode
             )

  (setq-hook! 'toml-mode-hook
    outline-regexp (rx (| (: (+ "[") (+? nonl) (+ "]"))
                          (: "# " (1+ "-") line-end)
                          (: "##--")
                          (: (+? nonl) "=" (+ space) "["
                             (or (: (+? space) (syntax comment-start))
                                 line-end))))
    outline-heading-end-regexp (rx (or (syntax comment-start) line-end))
    outline-level #'jg-toml-outline-level
    )
  )

(use-package! toml-ts-mode
  :commands toml-ts-mode
  :config
  ;; derived from text mode

  (add-hook! 'toml-ts-mode-hook :depth 99
             #'abbrev-mode
             #'treesit-fold-mode
             #'librarian-insert-minor-mode
             )

  (setq-hook! 'toml-ts-mode-hook
    outline-regexp (rx (or (: (+ "[") (+? nonl) (+ "]"))
                           (: (+? nonl) "=" (+ space) "["
                              (or (: (+? space) (syntax comment-start))
                                  line-end))))
    outline-heading-end-regexp (rx (or (: (syntax comment-start)) line-end))
    outline-level #'jg-toml-outline-level
    )
  )

;;
;;; config.el ends here
