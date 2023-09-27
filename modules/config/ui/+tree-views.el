;; +tree-views.el -*- mode: elisp; lexical-binding: t; -*-

(use-package! treemacs
  ;; :config
  ;; (add-hook 'treemacs-mode-hook #'treemacs-hide-gitignored-files-mode)
  )

(use-package! treemacs-evil :after treemacs)

(use-package! treemacs-projectile :after treemacs)

(use-package! treemacs-persp :after treemacs)

(use-package! neotree
  :commands (neotree-show neotree-hide neotree-toggle neotree-dir neotree-find neo-global--with-buffer neo-global--window-exists-p)
  :config
  (after! winner
    (add-to-list 'winner-boring-buffers neo-buffer-name))
  (add-hook! 'neo-enter-hook #'+neotree-fix-cursor-h)
  )
