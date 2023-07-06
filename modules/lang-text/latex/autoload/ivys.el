;;; ivys.el -*- lexical-binding: t; -*-

(defun +jg-latex-insert-package ()
  (interactive)
  (let ((insertfn #'(lambda (x) (insert "\\usepackage{" (car x) "}\n"))))
    (ivy-read "Insert Package: "
              LaTeX-global-package-files
              :action insertfn
              :multi-action (lambda (xs) (mapc insertfn xs))
              )
    )
  )

;;;###autoload
(defvar jg-latex-insert-ivys (make-hash-table :test 'equal))

(defvar jg-latex-dropcap-opts '(
                                "lines=      # <integer> 2 Num of lines to drop"
                                "depth=      # <integer> 0 Num of lines to reserve below"
                                "lhang=      # <decimal> 0 Width amt to hang into margin"
                                "loversize=  # <decimal> 0 Enlarge height"
                                "lraise=     # <decimal> 0 move vertically"
                                "findent=    # <dimen> 0pt gap between cap and indented text"
                                "nindent=    # <dimen> 0pt horizontal shift of indented lines"
                                "slope=      # <dimen> 0pt indent modify for A's and V's"
                                "ante=       # <string> nil prior-text to the cap"
                                "image       # <bool> false (requires graphicx). loads {letter name}.png/pdf/jpg... "
                                "grid=       # <bool> false round vertical skip above cap to intenger"
                                "novskip=    # <dimen> 1pt prevent skips above cap"
                                "realheight= # <bool> false "
                                ))

;;;###autoload
(defun +jg-latex-dropcap-opt-ivy ()
  (interactive)
  (insert (string-trim (car (s-split "#"
                                     (ivy-read "Dropcap Opt: "
                                               jg-latex-dropcap-opts
                                               :require-match t)
                                     t))))
  )
