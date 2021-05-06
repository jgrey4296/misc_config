;;; completion/jg-completion/+ivy_actions.el -*- lexical-binding: t; -*-

;; Overrides find-file's actions to add "f" for find literally
(ivy-add-actions 'counsel-find-file
                 '(("f" (lambda (x) (find-file-literally x)) "Fundamental")))
