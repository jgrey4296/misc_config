;;; compile-commands.el -*- lexical-binding: t; -*-

(defvar jg-latex-compile-program "pdflatex")
(defvar jg-latex-compile-args '("-interaction=nonstopmode"))

;;;###autoload
(defun +jg-latex-get-commands (&optional dir)
  (interactive)
  (-when-let* ((curr-file (buffer-file-name))
               (is-tex (f-ext? curr-file "tex"))
               )
    (+jg-projects-pair-cmds
       `("compile pdflatex"  ,(string-join (append (list "pdflatex")
                                          jg-latex-compile-args
                                          (list curr-file)) " "))
       `("compile lualatex"  ,(string-join (append (list "lualatex")
                                          jg-latex-compile-args
                                          (list curr-file)) " "))
       `("compile xelatex"  ,(string-join (append (list "xelatex")
                                          jg-latex-compile-args
                                          (list curr-file)) " "))

       `("clean"    ,(format "cd %s; ls" (f-parent curr-file)))
       '("install"  "tlmgr --usermode install --with-doc" :read)
       `("check"    ,(string-join (append (list jg-latex-compile-program)
                                          jg-latex-compile-args
                                          (list "-draftmode" curr-file)) " "))
       `("bibtex"      ,(format "bibtex --terse %s" (f-base curr-file)))
       ;; configs
       '("texdoc"   "texdoc -I" :read)
       '("package info" "tlmgr info" :read)
       '("version" "pdflatex --version")
       '("settings" "tlmgr conf")
       '("texdoc settings" "texdoc -f")
       '("update"  "tlmgr --usermode update --all")
       '("l3"       "fmtutil-user --all")
       '("find"     "kpsewhich -all" :read)
       '("fonts"   "updmap-user --listmaps")

       (when (f-exists? (f-swap-ext curr-file "pdf"))
         `("open"     ,(format "open -a Preview -nF %s" (f-swap-ext curr-file "pdf"))
           )
         )
     )
    )
  )

(defvar jg-latex-compile-quick-process nil)
(defvar jg-latex-compile-quick-file nil)

;;;###autoload
(defun +jg-latex-compile-file-quick ()
  (interactive)
  (when (and jg-latex-compile-quick-process (process-live-p jg-latex-compile-quick-process))
    (kill-process jg-latex-compile-quick-process))
  (-when-let (buff (get-buffer "*latex-check*"))
    (kill-buffer buff))

  (setq jg-latex-compile-quick-process (make-process
                                        :name "latex-quick"
                                        :buffer "*latex-check*"
                                        :noquery t
                                        :sentinel #'(lambda (proc stat)
                                                      (when (not (process-live-p proc))
                                                        (message "Latex Compile of %s Result: %s"
                                                                 jg-latex-compile-quick-file
                                                                 (process-exit-status proc)
                                                                 )
                                                        (when  (not (eq 0 (process-exit-status proc)))
                                                          (display-buffer "*latex-check*")
                                                          (save-window-excursion
                                                            (when (get-buffer-window "*latex-check*")
                                                              (with-selected-window (get-buffer-window "*latex-check*")
                                                                (goto-char (point-max))
                                                                (recenter))
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                        :stderr nil
                                        :command
                                        (append
                                         (list jg-latex-compile-program
                                               "-interaction=nonstopmode"
                                               )
                                         (list "-draftmode"
                                               (buffer-file-name))))
        jg-latex-compile-quick-file (f-base (buffer-file-name))
        )
  )