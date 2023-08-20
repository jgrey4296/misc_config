;;; lang/solidity/config.el -*- lexical-binding: t; -*-

(use-package! solidity-mode
  :config
  (setq solidity-comment-style 'slash)
  (spec-handling-add docsets '(solidity-mode "Solidity"))
  (spec-handling-add! company
                      '(solidity-mode (:mode . #'company-solidity))
                      )
  )

(use-package! solidity-flycheck  ; included with solidity-mode
  :defer t
  :after solidity-mode
  :config
  (setq flycheck-solidity-solc-addstd-contracts t)
  (when (funcall flycheck-executable-find solidity-solc-path)
    (add-to-list 'flycheck-checkers 'solidity-checker nil #'eq))
  (when (funcall flycheck-executable-find solidity-solium-path)
    (add-to-list 'flycheck-checkers 'solium-checker nil #'eq))
  )

(use-package! company-solidity
  :defer t
  :after solidity-mode
  :config (delq! 'company-solidity company-backends)
  )
