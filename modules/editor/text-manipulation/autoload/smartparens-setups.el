;;; smartparens-setups.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defun +default-cc-sp-point-is-template-p (id action context)
  "Return t if point is in the right place for C++ angle-brackets."
  (and (sp-in-code-p id action context)
       (cond ((eq action 'insert)
              (sp-point-after-word-p id action context))
             ((eq action 'autoskip)
              (/= (char-before) 32)))))

(defun +default-cc-sp-point-after-include-p (id action context)
  "Return t if point is in an #include."
  (and (sp-in-code-p id action context)
       (save-excursion
         (goto-char (line-beginning-position))
         (looking-at-p "[ 	]*#include[^<]+"))))

;;;###autoload
(defun +jg-text-setup-smartparens--cc-mode ()
  (setq-default c-electric-flag nil)
  (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
    (define-key c-mode-base-map key nil))

  ;; Smartparens and cc-mode both try to autoclose angle-brackets
  ;; intelligently. The result isn't very intelligent (causes redundant
  ;; characters), so just do it ourselves.
  (define-key! c++-mode-map "<" nil ">" nil)


  ;; ...and leave it to smartparens
  (sp-local-pair '(c++-mode objc-mode)
                 "<" ">"
                 :when '(+default-cc-sp-point-is-template-p
                         +default-cc-sp-point-after-include-p)
                 :post-handlers '(("| " "SPC")))

  (sp-local-pair '(c-mode c++-mode objc-mode java-mode)
                 "/*!" "*/"
                 :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))
  )


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 13, 2025
;; Modified:   January 13, 2025
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
;;; smartparens-setups.el ends here
