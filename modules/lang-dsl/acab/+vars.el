;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(defgroup acab-ide '() "Customization group for trie-related modes")

(defvar acab-ide/rule-helm-source nil
  "Main Helm Source for rule loading / finding ")
(defvar acab-ide/rule-helm-dummy-source)
(defvar acab-ide/type-helm-source nil
  "Main Helm Source for type loading / finding ")
(defvar acab-ide/type-helm-dummy-source)


(spec-handling-add! eval
                    `(acab-rule-mode ,#'acab-mode/open-repl)
                    )
