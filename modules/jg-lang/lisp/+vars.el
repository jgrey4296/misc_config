;;; +vars.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.el\\.gz" . emacs-lisp-mode))
(set-eval-handler! '(emacs-lisp-mode lisp-interaction-mode) #'+jg-lisp-eval)

;;-- emacs source paths
(after! (ffap find-func)
  (let ((paths-to-add (-concat
                       (ffap-all-subdirs "/Volumes/documents/github/emacs-src/lisp/" 1)
                       (ffap-all-subdirs (expand-file-name "straight/repos" doom-local-dir) 1)
                       (ffap-all-subdirs (expand-file-name "modules" doom-private-dir))
                       (ffap-all-subdirs (expand-file-name "packages" doom-private-dir)))))
  (mapc (lambda (x)
          (add-to-list 'find-library-source-path x))
        paths-to-add)
  )
  (setq find-function-C-source-directory "/Volumes/documents/github/emacs-src/src")

)
;;-- end emacs source paths

;;-- rotate text
(set-rotate-patterns! 'emacs-lisp-mode
  :symbols '(("t" "nil")
             ("let" "let*")
             ("when" "unless")
             ("advice-add" "advice-remove")
             ("defadvice!" "undefadvice!")
             ("add-hook" "remove-hook")
             ("add-hook!" "remove-hook!")
             ("it" "xit")
             ("describe" "xdescribe")
             )
  )
;;-- end rotate text

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