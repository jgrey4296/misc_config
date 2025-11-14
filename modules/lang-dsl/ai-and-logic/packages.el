;; packages.el -*- mode: elisp; lexical-binding: t; -*-

(package! instal-mode     :recipe (:host github :repo "jgrey4296/instal-stable"))
(package! agentspeak-mode :recipe (:host github :repo "jgrey4296/agentspeak-mode"))
(package! jacamo-mode     :recipe (:host github :repo "jgrey4296/jacamo-mode"))
(package! soar-mode       :recipe (:host github :repo "jgrey4296/soar-mode"))

(package! clips-mode)
(package! pasp-mode)
(package! z3-mode)
(package! sweeprolog)

(package! ob-prolog)
(package! ob-ceptre       :recipe (:host github :repo "jgrey4296/misc-modes" :files ("org-babels/ob-ceptre.el") :local-repo "misc-modes"))
(package! ob-clips        :recipe (:host github :repo "jgrey4296/misc-modes" :files ("org-babels/ob-clips.el") :local-repo "misc-modes"))
(package! ob-z3           :recipe (:host github :repo "jgrey4296/misc-modes" :files ("org-babels/ob-z3.el") :local-repo "misc-modes"))
