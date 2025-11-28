;;; +omit.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar jg-dired-omit-file-sources `(,(expand-file-name "~/.gitignore_global")
                                     ,(expand-file-name "tools/ignore/search_ignore" templates-loc)
                                     ))
(defvar jg-dired-omit-extensions '(".a" ".annot" ".aux"
                                   ".bbl" ".beam" ".bin" ".blg" ".bzr/"
                                   ".class" ".cm/" ".cma" ".cmi" ".cmo" ".cmt" ".cmti" ".cmx" ".cmxa" ".cp" ".cps"
                                   ".d64fsl" ".dfsl" ".dx32fsl" ".dx64fsl" ".dxl"
                                   ".elc"
                                   ".fas" ".fasl" ".fmt" ".fn" ".fns" ".fsl" ".fx32fsl" ".fx64fsl"
                                   ".git/" ".glo" ".glob" ".gmo"
                                   ".hg/" ".hi"
                                   ".idx"
                                   ".jam"
                                   ".ky" ".kys"
                                   ".la" ".lbin" ".lib" ".ln" ".lo" ".lof" ".lot" ".lx32fsl" ".lx64fsl"
                                   ".mem" ".mo"
                                   ".o"
                                   ".p64fsl" ".pfsl" ".pg" ".pgs" ".phi" ".pho" ".pyc" ".pyo"
                                   ".so" ".sparcf" ".svn/" ".sx32fsl" ".sx64fsl"
                                   ".tfm" ".toc" ".tp" ".tps" ".ufsl"
                                   ".vee" ".vo" ".vok" ".vos" ".vr" ".vrs"
                                   ".wx32fsl" ".wx64fsl"
                                   ".x86f"
                                   "CVS/" "_MTN/" "_darcs/" "~"
                                   ".gd.uid"
                                   )
  )

(rx-let ((filename (*? graph))
         (build-tools (| "gradlew" "gradlew.bat"
                         (: ?.
                            (| "gradle" "rustup" "doit.db.db" "mono"
                               (: "node" (? "_modules"))
                               (: "npm" (? "-global)"))))))
         (compiled (| (: filename (: ?. "o" "elc" "pyo"))))
         (configs  (| (: ?. (| (: filename "rc")
                               (: "project" (? "ile")) "venv" ))))
         (dotfiles (: ?. filename))
         (flycheck "flycheck_")
         (gtags    (| "GPATH" "GRTAGS" "GTAGS"))
         (java     (| (: filename ".class")))
         (latex    (| (: ?. (| "auctex-auto" ))))
         (lockfiles (: filename ".lock"))
         (logs     (| (: "log." filename)))
         (parent "^\\..*$")
         (prolog   (| (: ?. (| "swipl-dir-history" "swp" "swt" ))))
         (python   (| "__pycache__"
                      (: filename ".egg-info")
                      (: ?. (| "ipython" "jupyter" "matplotlib" "mypy.ini" ))))
         (ruby     (| (: ?. "gem")))
         (temp (: ?. "temp"))
         (vcs      (| (: ?. (| (:"git" (? "ignore")) "svn" ))))
         (workflows (: ?. (| "github" "cargo")))
         (misc-files (: (| "LICENSE"
                           "CHANGELOG"
                           "README"
                           "repo-layout"
                           )
                        (? ?. filename)))

         )
  (defvar jg-dired-omit-files (rx line-start (|
                                              dotfiles
                                              gtags
                                              build-tools
                                              python
                                              flycheck
                                              compiled
                                              java
                                              lockfiles
                                              temp
                                              misc-files
                                              )
                                  line-end
                                  )
    )
  (provide 'dired-omit-files-set)
  )

(speckler-setq! dired-omit ()
  dired-omit-files jg-dired-omit-files
  dired-omit-extensions jg-dired-omit-extensions
  )

;;; +omit.el ends here
