;;; test_load.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 09, 2023
;; Modified: April 09, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  For loading the rust module
;;
;;; Code:
;;-- end header

(add-to-list 'load-path (expand-file-name ".temp"))

(require 'jgtest)

(jgtest-say-hello "blah")

;;; test_load.el ends here
