;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: January 03, 2023
;; Modified: January 03, 2023
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
(load! "+funcs")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )
(after! (ivy counsel)
  (load! "+ivys")
  )

(use-package! project-walk
  :commands (project-walk-minor-mode project-walk-next)
)

(add-hook! 'jg-ui-reapply-hook #'+jg-projects-reapply-specs)
;;; config.el ends here
