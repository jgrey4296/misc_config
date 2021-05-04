;;; completion/jg-completion/+ivy_actions.el -*- lexical-binding: t; -*-

;; Overrides find-file's actions to add "f" for find literally
(ivy-set-actions 'counsel-find-file
                 '(("j" find-file-other-window "other window")
                   ("f" (lambda (x) (find-file-literally x)) "Fundamental")
                   ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
                   ("x" counsel-find-file-extern "open externally")
                   ("r" counsel-find-file-as-root "open as root")
                   ("R" find-file-read-only "read only")
                   ("k" counsel-find-file-delete "delete")
                   ("c" counsel-find-file-copy "copy file")
                   ("m" counsel-find-file-move "move or rename")
                   ("d" counsel-find-file-mkdir-action "mkdir")
                   ("p"
                    (lambda
                      (path)
                      (with-ivy-window
                        (insert
                         (file-relative-name path default-directory))))
                    "insert relative path")
                   ("P"
                    (lambda
                      (path)
                      (with-ivy-window
                        (insert path)))
                    "insert absolute path")
                   ("l"
                    (lambda
                      (path)
                      (with-ivy-window
                        (insert
                         (format "[[./%s]]"
                                 (file-relative-name path default-directory)))))
                    "insert relative org-link")
                   ("L"
                    (lambda
                      (path)
                      (with-ivy-window
                        (insert
                         (format "[[%s]]" path))))
                    "Insert absolute org-link")))
