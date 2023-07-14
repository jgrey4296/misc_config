;;; +testing.el -*- lexical-binding: t; -*-

(doom-log "Loading Python Testing")

(use-package! pyimport :after python-mode)

(use-package! py-isort
  :defer t
  :init
  (map! :after python
        :map python-mode-map
        :localleader
        (:prefix ("i" . "imports")
          :desc "Sort imports"      "s" #'py-isort-buffer
          :desc "Sort region"       "r" #'py-isort-region))
  )

(use-package! nose
  :disabled t
  :commands nose-mode
  :preface
  (defvar nose-mode-map (make-sparse-keymap))

  :minor ("/test_.+\\.py$" . nose-mode)
  :config
  (when (featurep 'evil)
    (add-hook 'nose-mode-hook #'evil-normalize-keymaps))
  )

(use-package! python-pytest
  :commands python-pytest-dispatch
)
