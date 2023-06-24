;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

(package! org
  :recipe (:host github
           ;; REVIEW: I intentionally avoid git.savannah.gnu.org because of SSL
           ;;   issues (see #5655), uptime issues, download time, and lack of
           ;;   shallow clone support.
           :repo "emacs-straight/org-mode"
           :files (:defaults "etc")
           :depth 1
           ;; HACK: Org has a post-install step that generates org-version.el
           ;;   and org-loaddefs.el, but Straight doesn't invoke this step, and
           ;;   the former doesn't work if the Org repo is a shallow clone.
           ;;   Rather than impose the network burden of a full clone (and other
           ;;   redundant work in Org's makefile), I'd rather fake these files
           ;;   instead. Besides, Straight already produces a org-autoloads.el,
           ;;   so org-loaddefs.el isn't needed.
           :build t
           :pre-build
           (progn
             (with-temp-file "org-loaddefs.el")
             (with-temp-file "org-version.el"
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents (doom-path "lisp/org.el") nil 0 1024)
                        (if (re-search-forward "^;; Version: \\([^\n-]+\\)" nil t)
                            (match-string-no-properties 1)
                          "Unknown"))))
                 (insert (format "(defun org-release () %S)\n" version)
                         (format "(defun org-git-version (&rest _) \"%s-??-%s\")\n"
                                 version (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                         "(provide 'org-version)\n")))))
 )
(package! org-contrib :recipe (:host github :repo "emacsmirror/org-contrib"))

(package! org-unit-test :recipe `(:local-repo ,(expand-file-name "packages/misc/org-unit-test" doom-user-dir)))


(package! org-drill)
(package! org-pomodoro)
(package! org-projectile)
(package! org-superstar)
(package! outline-toc)
(package! link-hint)
(package! graphviz-dot-mode)

(package! avy)
(package! htmlize)
(package! org-yt :recipe (:host github :repo "TobiasZawada/org-yt"))
(package! ox-clip)
(package! toc-org)
(package! org-cliplink)

;; TODO Adjust when this is added to GNU ELPA
(when (modulep! +contacts) (package! org-contacts :recipe (:host nil :type git :repo "https://repo.or.cz/org-contacts.git")))

(when (and IS-MAC (modulep! :os macos)) (package! org-mac-link))

(when (modulep! +passwords) (package! org-passwords :recipe (:host github :repo "alfaromurillo/org-passwords.el")))

(when (modulep! :editor evil +everywhere) (package! evil-org :recipe (:host github :repo "hlissner/evil-org-mode")))
(when (modulep! :tools pdf) (package! org-pdftools))
(when (modulep! :tools magit) (package! orgit))
(when (modulep! :tools magit +forge) (package! orgit-forge))
(when (modulep! +brain) (package! org-brain))
(when (modulep! +dragndrop) (package! org-download))
(when (modulep! +gnuplot) (package! gnuplot) (package! gnuplot-mode))
(when (modulep! +jupyter) (package! jupyter))
(when (modulep! +journal) (package! org-journal))
(when (modulep! +noter) (package! org-noter))
(when (modulep! +pomodoro) (package! org-pomodoro))
(when (modulep! +pretty) (package! org-appear) (package! org-superstar) (package! org-fancy-priorities))
(when (modulep! +present)
  (package! centered-window :recipe (:host github :repo "anler/centered-window-mode"))
  (package! org-tree-slide)
  (package! org-re-reveal)
  (package! revealjs :recipe (:host github :repo "hakimel/reveal.js" :files ("css" "dist" "js" "plugin")))
   )
(cond
 ((modulep! +roam)
  (package! org-roam
    :recipe (:host github :repo "org-roam/org-roam-v1")
   ))
 ((modulep! +roam2)
  (package! org-roam
    ;; FIXME A :recipe isn't strictly necessary, but without it, our package
    ;;       bumper fails to distinguish between org-roam v1 and v2.
    :recipe (:host github :repo "org-roam/org-roam")
   )))

;;; Babel
(package! ob-async)
(when (modulep! :lang clojure) (package! ob-clojure-literate :recipe (:type git :host nil :repo "https://repo.or.cz/ob-clojure-literate.el.git")))
(when (modulep! :lang crystal) (package! ob-crystal))
(when (modulep! :lang elixir) (package! ob-elixir))
(when (modulep! :lang fsharp) (package! ob-fsharp :recipe (:host github :repo "elken/ob-fsharp")))
(when (modulep! :lang go) (package! ob-go))
(when (modulep! :lang graphql) (package! ob-graphql))
(when (modulep! :lang hy) (package! ob-hy))
(when (modulep! :lang nim) (package! ob-nim))
(when (modulep! :lang php) (package! ob-php :recipe (:type git :host nil :repo "https://repo.or.cz/ob-php.git")))
(when (modulep! :lang racket) (package! ob-racket :recipe (:host github :repo "DEADB17/ob-racket")))
(when (modulep! :lang rest) (package! ob-restclient))
(when (modulep! :lang scala) (package! ob-ammonite))

;;; Export
(when (modulep! +pandoc) (package! ox-pandoc))
(when (modulep! +hugo) (package! ox-hugo :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)))
(when (modulep! :lang rst) (package! ox-rst))
(package! ox-html-epub :recipe `(:local-repo ,(expand-file-name "packages/ox-html-epub" doom-user-dir)))
