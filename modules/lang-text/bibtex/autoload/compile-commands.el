;;; compile-commands.el -*- lexical-binding: t; -*-

(defvar jg-bibtex-compile-args '())

;;;###autoload
(defun +jg-bibtex-get-commands (&optional dir)
  (interactive)
  (-when-let* ((curr-file (buffer-file-name))
               (rel-file (f-relative (f-no-ext (buffer-file-name)) (projectile-project-root)))
               (is-tex (f-ext? curr-file "bib"))
               )
    (+jg-eval--pair-cmds
       `("bibtex"      ,(format "bibtex --terse %s" rel-file))

       (when (f-exists? (f-swap-ext curr-file "pdf"))
         `("bibtex open"     ,(if (eq system-type 'darwin)
                           (format "open -a Preview -nF %s" (f-swap-ext curr-file "pdf"))
                         (format "open %s" (f-swap-ext curr-file "pdf"))
                         )
           )
         )
     )
    )
  )
