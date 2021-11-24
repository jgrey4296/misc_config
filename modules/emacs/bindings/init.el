;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: November 25, 2021
;; Modified: November 25, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/johngrey/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(message "Redefining doom--define-leader-key")

(defmacro doom--define-leader-key (&rest keys)
  (debug)
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
            (def (pop keys)))
        (if (keywordp key)
            (when (memq key '(:prefix :infix))
              (setq prefix def))
          (when prefix
            (setq key `(general--concat t ,prefix ,key)))
          (let* ((udef (cdr-safe (doom-unquote def)))
                 (bdef (if (general--extended-def-p udef)
                           (general--extract-def (general--normalize-extended-def udef))
                         def))
                 (use-def (cons 'list udef))
                 )
            (push `(define-leader-key! ,key ,use-def) forms))
          )))
    (macroexp-progn
     (append (and wkforms `((after! which-key ,@(nreverse wkforms))))
             (nreverse forms)))))


;;; init.el ends here
