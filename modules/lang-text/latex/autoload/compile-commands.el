;;; compile-commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-latex-get-commands (&optional dir)
  (interactive)
  (-when-let* ((curr-file (buffer-file-name))
               (is-tex (f-ext? curr-file "tex"))
               )
    (+jg-projects-pair-cmds
     `(
       ("compile" . ,(format "pdflatex -interaction=batchmode %s" curr-file))
       ("clean"   . "echo todo")
       ("check"   . ,(format "pdflatex -interaction=batchmode -draftmode %s" curr-file))
       ("bib"     . ,(format "bibtex %s" curr-file))
       ,(when (f-exists? (f-swap-ext curr-file "pdf"))
          `("open"    . ,(format "open -a Preview -nF %s" (f-swap-ext curr-file "pdf"))
            )
          )
       )
     )
    )
  )
