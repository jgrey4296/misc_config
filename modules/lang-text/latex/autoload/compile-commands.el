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
       `("compile"  ,(string-join (append (list jg-latex-compile-program)
                                          jg-latex-compile-args
                                          (list curr-file)) " "))
       '("clean"    "echo todo")
       '("install"  "tlmgr --usermode install" :read)
       '("l3"       "fmtutil-user --all")
       `("check"    ,(string-join (append (list jg-latex-compile-program)
                                          jg-latex-compile-args
                                          (list "-draftmode" curr-file)) " "))
       '("version" "pdflatex --version")
       `("bib"      ,(format "bibtex %s" curr-file))
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
