;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;; HACK A necessary hack because org requires a compilation step after being
;;      cloned, and during that compilation a org-version.el is generated with
;;      these two functions, which return the output of a 'git describe ...'
;;      call in the repo's root. Of course, this command won't work in a sparse
;;      clone, and more than that, initiating these compilation step is a
;;      hassle, so...
(add-hook! 'straight-use-package-pre-build-functions
  (defun +org-fix-package-h (package &rest _)
    (when (equal package "org-mode")
      (with-temp-file (expand-file-name "org-version.el" (straight--repos-dir "org-mode"))
        (insert "(fset 'org-release (lambda () \"9.4\"))\n"
                "(fset 'org-git-version #'ignore)\n"
                "(provide 'org-version)\n")))))

;; Install cutting-edge version of org-mode, and from a mirror, because
;; code.orgmode.org runs on a potato.
(package! org
  :recipe (:host github
           :repo "emacs-straight/org-mode"
           :files ("*.el" "lisp/*.el" "contrib/lisp/*.el"))
  )
  ;; :pin "220f2b0d93a6927eb673978c0042a1d4673e86aa"
  ;; Prevents built-in Org from sneaking into the byte-compilation of
  ;; `org-plus-contrib', and inform other packages that `org-mode' satisfies the
  ;; `org' dependency: https://github.com/raxod502/straight.el/issues/352
  ;; :shadow 'org)

(package! avy)
(package! htmlize )
(package! org-yt
  :recipe (:host github :repo "TobiasZawada/org-yt")
  )
(package! ox-clip )
(package! toc-org )
(package! org-cliplink )

(when (featurep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    ))
(when (featurep! :tools pdf)
  (package! org-pdftools ))
(when (featurep! :tools magit)
  (package! orgit ))
(when (featurep! +brain)
  (package! org-brain ))
(when (featurep! +dragndrop)
  (package! org-download ))
(when (featurep! +gnuplot)
  (package! gnuplot )
  (package! gnuplot-mode ))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython ))
(when (featurep! +jupyter)
  (package! jupyter ))
(when (featurep! +journal)
  (package! org-journal ))
(when (featurep! +noter)
  (package! org-noter ))
(when (featurep! +pomodoro)
  (package! org-pomodoro ))
(when (featurep! +pretty)
  (package! org-superstar )
  (package! org-fancy-priorities ))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    )
  (package! org-tree-slide )
  (package! org-re-reveal )
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    ))
(when (featurep! +roam)
  (package! org-roam )
  (when (featurep! :completion company)
    (package! company-org-roam )))

;;; Babel
(package! ob-async )
(when (featurep! :lang crystal)
  (package! ob-crystal ))
(when (featurep! :lang go)
  (package! ob-go ))
(when (featurep! :lang hy)
  (package! ob-hy ))
(when (featurep! :lang nim)
  (package! ob-nim ))
(when (featurep! :lang racket)
  (package! ob-racket
    :recipe (:host github :repo "DEADB17/ob-racket")
    ))
(when (featurep! :lang rest)
  (package! ob-restclient ))
(when (featurep! :lang scala)
  (package! ob-ammonite ))

;;; Export
(when (featurep! +pandoc)
  (package! ox-pandoc ))
(when (featurep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    ))
(when (featurep! :lang rst)
  (package! ox-rst ))
(package! cl-lib)
