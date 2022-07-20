;;; +file-templates.el -*- lexical-binding: t; -*-

(after! jg-file-templates
  ;; Lisp
  (set-file-templates!
   '(emacs-lisp-mode    :trigger "__package")
   '("\\.el$"           :when +file-templates-in-emacs-dirs-p :trigger "__doom-module" :mode emacs-lisp-mode)

   '("-test\\.el$"      :mode emacs-ert-mode)
   '("/.dir-locals.el$" :mode emacs-lisp-mode :trigger "__dir_locals")
   '("ob-.+?\\.el$"     :mode emacs-lisp-mode :trigger "__org_babel")
   '("mode\\.el$"       :trigger "__mode" :mode emacs-lisp-mode)
   '("minor-mode\\.el$" :trigger "__minor-mode" :mode emacs-lisp-mode)
   '("packages\\.el$"   :when +file-templates-in-emacs-dirs-p :trigger "__doom_packages" :mode emacs-lisp-mode)
   '("config\\.el$"     :when +file-templates-in-emacs-dirs-p :trigger "__doom_config"   :mode emacs-lisp-mode)
   '("/doctor\\.el$"    :when +file-templates-in-emacs-dirs-p :trigger "__doom-doctor"   :mode emacs-lisp-mode)
   '("/test/.+\\.el$"   :when +file-templates-in-emacs-dirs-p :trigger "__doom-test"     :mode emacs-lisp-mode)
   )
)
