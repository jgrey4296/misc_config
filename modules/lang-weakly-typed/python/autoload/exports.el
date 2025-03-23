;; exports.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header
(require 's)

(defconst jg-python-export-template (s-join
                                     "\n" '(
                                            "# ##-- Generated Exports"
                                            "__all__ = ( # noqa: RUF022"
                                            "%s" "%s" "%s"
                                            ")"
                                            "# ##-- end Generated Exports"
                                            )
                                     ))
(defconst jg-python-export-search-re (rx line-start
                                         (group (| "type" "class" "def"))
                                         (group (+? nonl))
                                         (any "=[(:")
                                        ))
(defconst jg-python-export-line-len 50)

;;;###autoload
(defun +jg-python-gen-exports ()
  "Generate an __all__ value from all defined types and classes in the file"
  (interactive)
  (let ((base-text (buffer-string))
        (wrapper (-rpartial #'s-wrap "\"" "\""))
        ;; words
        types
        classes
        fns
        ;; plist of lines
        lines
        )
    (with-temp-buffer
      (insert base-text)
      (vimish-fold-unfold-all)
      ;; Grab the values
      (goto-char (point-min))
      (while (re-search-forward jg-python-export-search-re nil t)
        (pcase (match-string 1)
          ("type"   (push (s-trim (match-string-no-properties 2)) types))
          ("class"  (push (s-trim (match-string-no-properties 2)) classes))
          ("def"    (push (s-trim (match-string-no-properties 2)) fns))
          )
        )
      )
    ;; Split into lines of jg-py-ex-l-len
    (cl-loop for targets in `((:types     . ,types)
                              (:classes   . ,classes)
                              (:fns       . ,fns))
             for source = (cdr targets)
             for result = nil
             do
             (message "Handling: %s : %s" targets source)
             (cl-loop for word in (sort source #'string-lessp)
                      with line  = nil
                      with total = 0
                      for wlen   = (length word)
                      do (cl-incf total wlen)
                      (push (funcall wrapper word) line)
                      if (< jg-python-export-line-len total)
                      do (push (s-join ", " (reverse line)) result)
                      and do (setq line nil total 0)
                      finally
                      (when line (push (s-join ", " (reverse line)) result))
                      )
             (push (cons (car targets) result) lines)
             )
    ;; insert
    (insert (format jg-python-export-template
                    (if-let ((vals (alist-get :types lines)))
                        (s-wrap (s-join ",\n" (reverse vals )) "# -- Types\n" ",")
                      "")
                    (if-let ((vals (alist-get :classes lines)))
                        (s-wrap (s-join ",\n" (reverse vals )) "# -- Classes\n" ",")
                      "")
                    (if-let ((vals (alist-get :fns lines)))
                        (s-wrap (s-join ",\n" (reverse vals )) "# -- Functions\n" ",")
                      "")
                    ))
    )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    March 23, 2025
;; Modified:   March 23, 2025
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
;;; exports.el ends here
