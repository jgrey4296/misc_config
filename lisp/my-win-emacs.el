;Personalised .emacs file
;John Grey, Wed May 28 17:50:31 PDT 2014

;(setq debug-on-error 1)

;; Load paths
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(setq load-path (cons "~/.emacs.setup/lisp-lib/" load-path))

(let ((default-directory (expand-file-name "~/.emacs.setup/")))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path (expand-file-name "~/.emacs.setup/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.setup/lisp-lib"))

(load "my-functions")
(load "my-aliases")
(load "my-variables")
(load "my-keybindings")
(load "my-win-modes")
(load "my-hooks")
(load "my-colours")



