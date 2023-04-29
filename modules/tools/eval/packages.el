;; -*- no-byte-compile: t; -*-
;;; tools/eval/packages.el

(package! quickrun)
(when (modulep! +overlay)
  (package! eros))
