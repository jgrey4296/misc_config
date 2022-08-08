;;; +vars.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.el\\.gz" . emacs-lisp-mode))


;;-- fold spec
(after! jg-fold-specs
  (setq jg-lisp-fold-spec `((emacs-lisp-mode lisp-mode)
                            :open-all   hs-show-all
                            :close-all  hs-hide-all
                            :toggle     hs-toggle-hiding
                            :open       hs-show-block
                            :open-rec   nil
                            :close      hs-hide-block))
  (push jg-lisp-fold-spec evil-fold-list)
  )
;;-- end fold spec


;;-- file templates
(after! jg-file-templates
  ;; Lisp
  (+jg-completion-add-file-templates
   'lisp
   '(("/test/.+\\.el$"   :when +file-templates-in-emacs-dirs-p :trigger "__doom-test"     :mode emacs-lisp-mode)
     ("/doctor\\.el$"    :when +file-templates-in-emacs-dirs-p :trigger "__doom-doctor"   :mode emacs-lisp-mode)
     ("config\\.el$"     :when +file-templates-in-emacs-dirs-p :trigger "__doom_config"   :mode emacs-lisp-mode)
     ("packages\\.el$"   :when +file-templates-in-emacs-dirs-p :trigger "__doom_packages" :mode emacs-lisp-mode)
     ("minor-mode\\.el$" :trigger "__minor-mode" :mode emacs-lisp-mode)
     ("mode\\.el$"       :trigger "__mode"       :mode emacs-lisp-mode)
     ("ob-.+?\\.el$"     :mode emacs-lisp-mode :trigger "__org_babel")
     ("/.dir-locals.el$" :mode emacs-lisp-mode :trigger "__dir_locals")
     ("-test\\.el$"      :mode emacs-ert-mode)
     ("\\.el$"           :when +file-templates-in-emacs-dirs-p :trigger "__doom-module" :mode emacs-lisp-mode)
     (emacs-lisp-mode    :trigger "__package")
     )
   )
  (+jg-completion-activate-file-templates)
)
;;-- end file templates
