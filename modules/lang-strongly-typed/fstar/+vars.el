;;; +vars.el -*- lexical-binding: t; -*-


(set-lookup-handlers! 'fstar-mode
    :definition #'fstar-jump-to-definition
    :documentation #'fstar-doc-at-point-dwim
    )
