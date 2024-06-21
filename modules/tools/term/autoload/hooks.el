;;; hooks.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-remove-comint-capf ()
  (remove-hook 'completion-at-point-functions 'comint-completion-at-point t)
  (add-hook 'completion-at-point-functions 'comint-dynamic-complete-filename nil t)
  )


;;;###autoload
(defun +jg-term-shell-mode-setup-local-hook-h ()
  "Add a buffer local hook for moving to the prompt"
  (add-hook 'evil-normal-state-entry-hook
            (-partial #'comint-next-prompt 1)
            0
            t)
  )


;;-- Footer
;; Copyright (C) 2024 john grey
;;
;; Author:     john grey <https://github.com/jgrey4296>
;; Maintainer: john grey <jgrey4296@gmail.com>
;; Created:    June 10, 2024
;; Modified:   June 10, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; hooks.el ends here
