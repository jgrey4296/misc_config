;;; compile-commands.el -*- lexical-binding: t; -*-

(defvar jg-bibtexcompile-program "pdflatex")

(defvar jg-bibtex-compile-args '("-interaction=nonstopmode"))

(defvar jg-bibtexcompile-quick-process nil)

(defvar jg-bibtex-compile-quick-file nil)

;;;###autoload
(defun +jg-bibtex-get-commands (&optional dir)
  (interactive)
  (-when-let* ((curr-file (buffer-file-name))
               (rel-file (f-relative (f-no-ext (buffer-file-name)) (projectile-project-root)))
               (is-tex (f-ext? curr-file "tex"))
               )
    (+jg-projects-pair-cmds
       `("compile pdflatex"  ,(string-join (append (list "pdflatex" (format "-output-directory=%s" default-directory))
                                          jg-bibtex-compile-args
                                          (list curr-file)) " "))
       `("compile lualatex"  ,(string-join (append (list "lualatex" (format "-output-directory=%s" default-directory))
                                          jg-bibtex-compile-args
                                          (list curr-file)) " "))
       `("compile xelatex"  ,(string-join (append (list "xelatex" (format "-output-directory=%s" default-directory))
                                          jg-bibtex-compile-args (list curr-file)) " "))
       `("bibtex"      ,(format "bibtex --terse %s" rel-file))
       `("biblatex"    ,(format "biber %s" rel-file))
       `("check"    ,(string-join (append (list jg-bibtexcompile-program)
                                          jg-bibtex-compile-args
                                          (list "-draftmode" curr-file)) " "))

       `("clean"        ,(format "cd %s; find . -maxdepth 1 \\( -name '%2$s.log' -o -name '%2$s.aux' -o -name '%2$s.bbl' -o -name '%2$s.blg' -o -name '%2$s.out' -o -name '%2$s.pdf'  \\) -delete" (f-parent curr-file) (f-base curr-file)))
       `("clean-all"    ,(format "cd %s; find . -maxdepth 1 \\( -name '*.log' -o -name '*.aux' -o -name '*.bbl' -o -name '*.blg' -o -name '*.out' -o -name '*.pdf'  \\) -delete" (f-parent curr-file)))

       '("install"  ,(format "tlmgr %s install --with-doc" (if (eq 'darwin system-type) "--usermode" "")) :read)
       '("on-fly-pdflatex" "texliveonfly --compiler=pdflatex")
       `("on-fly-xelatex" ,(format "texliveonfly --compiler=xelatex %s" curr-file) :interactive)
       `("on-fly-lualatex" ,(format "texliveonfly --compiler=lualatex %s" curr-file) :interactive)
       ;; configs
       '("texdoc"   "texdoc -I" :read)
       '("package info" "tlmgr info" :read)
       '("version" "pdflatex --version")
       '("settings" "tlmgr conf")
       '("texdoc settings" "texdoc -f")
       '("update"  ,(format "tlmgr %s update --all" (if (eq 'darwin system-type) "--usermode" "")))
       '("l3"       "fmtutil-user --all")
       '("find"     "kpsewhich -all" :read)
       '("fonts"   "updmap-user --listmaps")
       `("system-fonts" ,(format "system_profiler -json SPFontsDataType > %s" (expand-file-name "~/.cache/fonts/fonts.json")))

       (when (f-exists? (f-swap-ext curr-file "pdf"))
         `("open"     ,(if (eq system-type 'darwin)
                           (format "open -a Preview -nF %s" (f-swap-ext curr-file "pdf"))
                         (format "open %s" (f-swap-ext curr-file "pdf"))
                         )
           )
         )
     )
    )
  )

;;;###autoload
(defun +jg-bibtex-compile-file-quick ()
  "  "
  (interactive)
  (when (and jg-bibtexcompile-quick-process (process-live-p jg-bibtexcompile-quick-process))
    (kill-process jg-bibtexcompile-quick-process))
  (-when-let (buff (get-buffer "*latex-check*"))
    (kill-buffer buff))

  (setq jg-bibtexcompile-quick-process (make-process
                                        :name "latex-quick"
                                        :buffer "*latex-check*"
                                        :noquery t
                                        :sentinel #'(lambda (proc stat)
                                                      (when (not (process-live-p proc))
                                                        (message "Latex Compile of %s Result: %s"
                                                                 jg-bibtex-compile-quick-file
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
                                         (list jg-bibtexcompile-program
                                               "-interaction=nonstopmode"
                                               )
                                         (list "-draftmode"
                                               (buffer-file-name))))
        jg-bibtex-compile-quick-file (f-base (buffer-file-name))
        )
  )
