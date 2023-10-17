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
             (with-temp-file "org-loaddefs.el"
               (insert "(provide 'org-loaddefs)")
               )
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

(package! org-unit-test :recipe (:host github :repo "jgrey4296/misc-modes" :files ("minor-mode/org-unit-test/*.el")))


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
(package! org-contacts :recipe (:host nil :type git :repo "https://repo.or.cz/org-contacts.git"))

(when (and IS-MAC (modulep! :os macos)) (package! org-mac-link))

(package! org-passwords :recipe (:host github :repo "alfaromurillo/org-passwords.el"))

(package! evil-org :recipe (:host github :repo "hlissner/evil-org-mode"))
(package! orgit)
(package! orgit-forge)
(package! org-brain)
(package! gnuplot)
(package! gnuplot-mode)
(package! org-journal)
(package! org-noter)
(package! org-appear)
(package! org-fancy-priorities)
(package! centered-window :recipe (:host github :repo "anler/centered-window-mode"))
(package! org-tree-slide)

;;; Babel
(package! ob-async)
(package! ob-elixir)
(package! ob-fsharp :recipe (:host github :repo "elken/ob-fsharp"))
(package! ob-graphql)
(package! ob-racket :recipe (:host github :repo "DEADB17/ob-racket"))
(package! ob-restclient)
(package! ob-ammonite)

;;; Export
(package! ox-pandoc)
(package! ox-hugo :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t))
(package! ox-rst)
(package! ox-epub :recipe (:host github :repo "jgrey4296/misc-modes" :files ("ox-epub/*.el") :local-repo "misc-modes"))
