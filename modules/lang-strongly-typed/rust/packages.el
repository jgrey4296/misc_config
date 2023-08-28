;; -*- no-byte-compile: t; -*-
;;; lang/rust/packages.el

(package! rustic)
(package! racer)
(package! rust-mode)
;; (package! llvm :type 'local :recipe (:host nil :local-repo "/usr/local/opt/llvm/share/emacs/site-lisp/lvvm/" :files ("*.el") :includes (llvm-mode)))
