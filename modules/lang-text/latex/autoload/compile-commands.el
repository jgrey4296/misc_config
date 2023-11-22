;;; compile-commands.el -*- lexical-binding: t; -*-

(defvar jg-latex-compile-program "pdflatex")
(defvar jg-latex-compile-args '("-interaction=nonstopmode"))
(defvar jg-latex-compile-search-re "^%% compiler:\\(.+\\)$")

;;;###autoload
(defun +jg-latex-get-commands (&optional dir)
  (interactive)
  (-when-let* ((curr-file (buffer-file-name))
               ;; (rel-file (f-relative (f-no-ext (buffer-file-name)) (projectile-project-root)))
               (rel-file (f-no-ext (f-filename (buffer-file-name))))
               (build-dir (f-parent (buffer-file-name)))
               (is-tex (f-ext? curr-file "tex"))
               (compile-with (save-excursion
                               (goto-char (point-min))
                               (re-search-forward jg-latex-compile-search-re)
                               (or (match-string 1) jg-latex-compile-program)
                               ))
               (output-dir (format "-output-directory=%s" default-directory))
               (compile-cmd (string-join (append (list compile-with output-dir) jg-latex-compile-args (list curr-file)) " "))

               )
    (+jg-projects-pair-cmds
     `("compile" ,compile-cmd)
     ;; `("bibtex"      ,(format "bibtex --terse %s" rel-file))
     `("bibtex"      ,(format "cd %s; bibtex --terse %s" build-dir rel-file))
     `("full" ,(format "cd %s; %s; bibtex --terse %s; %s; %s" build-dir compile-cmd rel-file compile-cmd compile-cmd ))
     `("biblatex"    ,(format "biber %s" rel-file))
     `("check"    ,(string-join (append (list jg-latex-compile-program)
                                        jg-latex-compile-args
                                        (list "-draftmode" curr-file)) " "))

     `("clean"        ,(format "cd %s; find . -maxdepth 1 \\( -name '%2$s.log' -o -name '%2$s.aux' -o -name '%2$s.bbl' -o -name '%2$s.blg' -o -name '%2$s.out' -o -name '%2$s.pdf'  \\) -delete" (f-parent curr-file) (f-base curr-file)))
     `("clean-all"    ,(format "cd %s; find . -maxdepth 1 \\( -name '*.log' -o -name '*.aux' -o -name '*.bbl' -o -name '*.blg' -o -name '*.out' -o -name '*.pdf'  \\) -delete" (f-parent curr-file)))

     `("install"  ,(format "tlmgr %s install --with-doc" (if (eq 'darwin system-type) "--usermode" "")) :read)
     `("info"     ,(format "tlmgr %s info --list " (if (eq 'darwin system-type) "--usermode" "")) :read)
     `("on-fly-pdflatex" "texliveonfly --compiler=pdflatex")
     `("on-fly-xelatex" ,(format "texliveonfly --compiler=xelatex %s" curr-file) :interactive)
     `("on-fly-lualatex" ,(format "texliveonfly --compiler=lualatex %s" curr-file) :interactive)
     ;; configs
     '("def"      "latexdef" :read)
     '("texdoc"   "texdoc -I" :read)
     '("package info" "tlmgr info" :read)
     '("version" "pdflatex --version")
     '("settings" "tlmgr conf")
     '("texdoc settings" "texdoc -f")
     `("update"  ,(format "tlmgr %s update --all" (if (eq 'darwin system-type) "--usermode" "")))
     `("l3"       ,(format "fmtutil %s --all" (if (eq 'darwin system-type) "-user" "--sys")))
     `("find"     "kpsewhich -all" :read)
     `("fonts"   "updmap-user --listmaps")
     `("system-fonts" ,(format "system_profiler -json SPFontsDataType > %s" (expand-file-name "~/.cache/fonts/fonts.json")))

     (when (f-exists? (f-swap-ext curr-file "pdf"))
       `("open"     ,(if (eq 'darwin system-type)
                         (format "open -a Preview -nF %s" (f-swap-ext curr-file "pdf"))
                       (format "evince %s" (f-swap-ext curr-file "pdf")))
         )
       )
     )
    )
  )

(defvar jg-latex-compile-quick-process nil)
(defvar jg-latex-compile-quick-file nil)

;;;###autoload
(defun +jg-latex-compile-file-quick ()
  "  "
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
