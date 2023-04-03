;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 23, 2023
;; Modified: March 23, 2023
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
(load! "+yasnippet")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(define-advice doom-snippets-initialize (:override () +jg-snippets-fix)
  nil
  )

;;-- hook setup
(add-hook 'jg-ui-reapply-hook      #'+jg-snippets-reapply-file-specs)
(add-hook 'doom-switch-buffer-hook #'+file-templates-check-h)
;;-- end hook setup

(after! jg-snippets-applied
  (require 'yasnippet)
  (advice-add '+snippet--completing-read-uuid :override #'+jg-snippets--completing-read-uuid)
  (add-hook 'yas-prompt-functions #'+jg-snippets-yas-prompt-fn -90)
  (yas-reload-all)
  )

;;; config.el ends here
