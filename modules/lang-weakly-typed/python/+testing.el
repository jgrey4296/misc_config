;;; +test.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(advice-add 'python-pytest--run             :filter-args #'+jg-python-test-extra-args)

(use-package! python-pytest
  :after python
  )

(use-package! python-coverage
  :after python
  :config

  (setq-hook! 'python-mode-hook
    python-coverage-default-file-name ".temp/coverage/coverage.xml"
    )

  )

(use-package! tox
  :when (modulep! +tox)
  :after python
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 11, 2025
;; Modified:   July 11, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; +test.el ends here
