(package! acab-ide :recipe `(:local-repo ,(expand-file-name  "~/github/python/acab/emacs") :files ("*.el" "data-struct/*.el" "modes/*.el" "util/*.el")))
(package! font-lock+ :recipe (:host github :repo "emacsmirror/font-lock-plus"))
(package! parsec)
