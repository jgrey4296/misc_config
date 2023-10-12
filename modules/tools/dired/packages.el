;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diff-hl)
(package! dired :built-in t)
(package! epa-dired :built-in t)
(package! dired-git-info)
(package! dired-quick-sort)
(package! dired-rsync)
(package! diredfl)
(package! fd-dired)
(when (modulep! +dirvish) (package! dirvish))
