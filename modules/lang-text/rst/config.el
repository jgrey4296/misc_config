;;; lang/rst/config.el -*- lexical-binding: t; -*-

(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package! sphinx-mode
  :hook (rst-mode . sphinx-mode))

(use-package! rst
  )

(use-package! rst
  :defer t
  :config
  (map! :localleader
        :map rst-mode-map
        (:prefix ("a" . "adjust")
          "a" #'rst-adjust
          "r" #'rst-adjust-region)
        (:prefix ("t" . "table of contents")
          "t" #'rst-toc
          "i" #'rst-toc-insert
          "u" #'rst-toc-update
          "f" #'rst-toc-follow-link))

  (add-hook 'rst-mode-hook #'(lambda ()
                               (setq-local yas-indent-line nil)
                               ))

  )
