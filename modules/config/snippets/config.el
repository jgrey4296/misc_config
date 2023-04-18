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
(add-hook 'doom-switch-buffer-hook #'+file-templates-check-h)
;;-- end hook setup

(spec-handling-setq! snippets
                     +file-templates-dir jg-snippets-file-templates-dir
                     +snippets-dir       jg-snippets-code-templates-dir
                     yas-snippet-dirs    (-filter #'identity (list +snippets-dir +file-templates-dir doom-snippets-dir yasnippet-snippets-dir))
                     yas--default-user-snippets-dir jg-snippets-code-templates-dir
                     )

(spec-handling-new! file-templates +file-templates-alist t append
                    (cl-loop for rule in vars
                             for priority = (* -1 (or (plist-get rule :priority) 0))
                             for clean    = (cl-loop for (k v) on body by #'cddr
                                                     unless (eq k :priority)
                                                     collect k and collect v)
                             collect (cons priority (cons head clean))
                             )
                    )

(after! jg-snippets-applied
  (advice-add '+snippet--completing-read-uuid :override #'+jg-snippets--completing-read-uuid)
  (add-hook 'yas-prompt-functions #'+jg-snippets-yas-prompt-fn -90)
  (yas-reload-all)
  )

;;; config.el ends here
