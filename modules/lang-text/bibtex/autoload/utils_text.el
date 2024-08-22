;;; utils_text.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-bibtex-title-case (x)
  "Split a string into separated words, and capitalise the first letter of each
before rejoining "
  (let* ((case-fold-search nil)
         (orig-syntax-table (syntax-table))
         (mod-syn (copy-syntax-table orig-syntax-table))
        )

    (condition-case err
        (progn
          ;; https://stackoverflow.com/questions/1314670
          (modify-syntax-entry ?’ "w" mod-syn)
          (modify-syntax-entry ?' "w" mod-syn)
          (set-syntax-table mod-syn)
          (string-join (cl-loop for x in (split-string x " +" t " +")
                                collect
                                (cond (t (capitalize x))
                                      ((s-matches-p (rx word-start (or "and" (+ upper-case)) word-end ) x)
                                       x)
                                      (t (capitalize x))
                                      )
                                )
                       " ")
          )
      (t (set-syntax-table orig-syntax-table)
         (signal (car err) (cdr err)))
      (t (set-syntax-table orig-syntax-table)
         err
         )
      )
    )
  )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 09, 2024
;; Modified:   February 09, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; utils_text.el ends here

(ert-deftest jg-bibtex-test-title-case ()
  "Tests the title case cleaning "
  (should (string-equal "This Is A Test" (+jg-bibtex-title-case "this is a test")))
  (should (string-equal "This Is A Test" (+jg-bibtex-title-case "This Is A Test")))
  (should (string-equal "Why Aren’t We Talking About Trump’s Fascism?" (+jg-bibtex-title-case "Why Aren’t We Talking About Trump’s Fascism?")))
  (should (string-equal "Why Aren't We Talking About Trump’s Fascism?" (+jg-bibtex-title-case "Why Aren't We Talking About Trump’s Fascism?")))
  (should (string-equal "Why Aren't 'we Blah' Talking About Trump’s Fascism?" (+jg-bibtex-title-case "Why Aren't 'We blah' Talking About Trump’s Fascism?")))
)
