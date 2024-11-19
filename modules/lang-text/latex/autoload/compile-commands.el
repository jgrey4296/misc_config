;;; compile-commands.el -*- lexical-binding: t; -*-

(defvar jg-latex-compile-quick-process nil)
(defvar jg-latex-compile-quick-file nil)


;;;###autoload
(defun +jg-latex-get-commands (&optional dir)
  "Builds compile commands for latex"
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
               (root (let ((proj-root (projectile-project-root))
                           (target (f-join (or (projectile-project-root) "doesnt exist") ".temp/export/tex/"))
                           )
                       (if (f-exists? target) target default-directory)))
               (output-file (f-join root (f-swap-ext (f-filename curr-file) "pdf")))
               (output-dir (format "-output-directory=%s" root))
               (compile-cmd (string-join (append (list compile-with output-dir) jg-latex-compile-args (list curr-file)) " "))

               )
    (+jg-eval--pair-cmds
     `("tex compile"        ,compile-cmd)
     ;; `("bibtex"      ,(format "bibtex --terse %s" rel-file))
     `("tex->bibtex"         ,(format "cd %s; bibtex --terse %s" build-dir rel-file))
     `("tex full"           ,(format "cd %s; %s; bibtex --terse %s; %s; %s" build-dir compile-cmd rel-file compile-cmd compile-cmd ))
     `("tex->biblatex"       ,(format "biber %s" rel-file))
     `("tex-check"          ,(string-join (append (list jg-latex-compile-program)
                                        jg-latex-compile-args
                                        (list "-draftmode" curr-file)) " "))

     `("tex-clean"        ,(format "cd %s; find . -maxdepth 1 \\( -name '%2$s.log' -o -name '%2$s.aux' -o -name '%2$s.bbl' -o -name '%2$s.blg' -o -name '%2$s.out' -o -name '%2$s.pdf'  -o -name '%2$s.sta' -o -name '%2$s.toc' \\) -delete" root (f-base curr-file)))
     `("tex-clean-all"    ,(format "cd %s; find . -maxdepth 1 \\( -name '*.log' -o -name '*.aux' -o -name '*.bbl' -o -name '*.blg' -o -name '*.out' -o -name '*.pdf' -o -name '*.sta' -o -name '*.toc' \\) -delete" root))

     `("tex install"      ,(format "tlmgr %s install --with-doc" (if (eq 'darwin system-type) "--usermode" "")) :read)
     `("tex info"         ,(format "tlmgr %s info --list " (if (eq 'darwin system-type) "--usermode" "")) :read)
     `("on-fly-pdflatex" "texliveonfly --compiler=pdflatex")
     `("on-fly-xelatex" ,(format "texliveonfly --compiler=xelatex %s" curr-file) :interactive)
     `("on-fly-lualatex" ,(format "texliveonfly --compiler=lualatex %s" curr-file) :interactive)
     ;; configs
     '("tex def"                                "latexdef" :read)
     '("texdoc"                                 "texdoc -w -I" :read)
     '("tex package info"                       "tlmgr info" :read)
     '("tex version"                            "pdflatex --version")
     '("tex settings"                           "tlmgr conf")
     '("tex doc settings"                    "texdoc -f")
     `("tex update"  ,(format "tlmgr %s update --all" (if (eq 'darwin system-type) "--usermode" "")))
     `("tex l3"       ,(format "fmtutil %s --all" (if (eq 'darwin system-type) "-user" "--sys")))
     `("tex find"                               "kpsewhich -all" :read)
     `("tex fonts"                              "updmap-user --listmaps")
     `("tex font system" ,(format              "system_profiler -json SPFontsDataType > %s" (expand-file-name "~/_cache_/fonts/fonts.json")))

     (when (f-exists? output-file)
       `("tex open"     ,(if (eq 'darwin system-type)
                         (format "open -a Preview -nF %s" output-file)
                       (format "evince %s" output-file)))
       )
     )
    )
  )

;;;###autoload
(defun +jg-latex-compile-file-quick ()
  "  "
  (interactive)
  (when (and jg-latex-compile-quick-process (process-live-p jg-latex-compile-quick-process)))
  (-when-let (buff (get-buffer "*latex-check*"))
    (kill-buffer buff))

  (basic-save-buffer)
  (let* ((curr-file (buffer-file-name))
         (compile-with (save-excursion
                         (goto-char (point-min))
                         (re-search-forward jg-latex-compile-search-re)
                         (or (match-string 1) jg-latex-compile-program)
                         ))
         (root (if (projectile-project-root)
                   (f-join (projectile-project-root) ".temp/tex/")
                 default-directory))
         (output-dir (format "-output-directory=%s" root))
         (compile-cmd (append (list compile-with output-dir) jg-latex-compile-args))
         )
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
                                           compile-cmd
                                           (list "-draftmode"
                                                 (buffer-file-name))))
          jg-latex-compile-quick-file (f-base (buffer-file-name))
          )
    )
  )
