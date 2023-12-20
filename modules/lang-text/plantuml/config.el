;;; lang/plantuml/config.el -*- lexical-binding: t; -*-

;; TODO link to compile

(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! plantuml-mode
  :commands (plantuml-mode plantuml-download-jar)
  :init
  (setq plantuml-jar-path (concat doom-data-dir "plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path)
  :config
  (setq plantuml-default-exec-mode
        (cond ((file-exists-p plantuml-jar-path) 'jar)
              ((executable-find "plantuml") 'executable)
              (plantuml-default-exec-mode)))

  (add-hook! 'plantuml-mode-hook
             #'general-insert-minor-mode
             )
  )

(use-package! flycheck-plantuml
  :after plantuml-mode
  :config
  (flycheck-plantuml-setup)
  )

(use-package! ob-plantuml
  :after plantuml-mode
  :config
  (unless (boundp 'org-babel-default-header-args:plantuml)
    (defvar org-babel-default-header-args:plantuml nil))
  (add-to-list 'org-babel-default-header-args:plantuml '(:cmdline . "-charset utf-8"))
  )
