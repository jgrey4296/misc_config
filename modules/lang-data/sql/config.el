;;; config.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(dlog! "Config SQL")

(local-load! "+bindings")

(use-package! sql-mode     :defer t)
(use-package! sqlite-mode  :defer t)
;; (use-package! esqlite-helm :defer t)


(speckler-add! org-src ()
  '(sql
    ("sqlite" . sql)
    ("sql" . sql)
    )
  )
(speckler-add! babel ()
  '(sql
    (:name sql        :lib ob-sql)
    (:name sqlite     :lib ob-sqlite)
    )
  )
(speckler-add! tree-sitter-lang ()
  '(sql-mode . sql)
  )
(speckler-add! treesit-source ()
  '(sql           "git@github.com:DerekStride/tree-sitter-sql.git")
  )

;;-- Footer
;; Copyright (C) 2023 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 16, 2023
;; Modified:   December 16, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; config.el ends here
