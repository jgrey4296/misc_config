;;; +vars.el -*- lexical-binding: t; -*-

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and variables.")

(defvar +emacs-lisp-outline-regexp "[ \t]*;;;\\(;*\\**\\) [^ \t\n]"
  "Regexp to use for `outline-regexp' in `emacs-lisp-mode'.
This marks a foldable marker for `outline-minor-mode' in elisp buffers.")

(defvar +emacs-lisp-linter-warnings
  '(not free-vars    ; don't complain about unknown variables
        noruntime    ; don't complain about unknown function calls
        unresolved)  ; don't complain about undefined functions
  "The value for `byte-compile-warnings' in non-packages.

This reduces the verbosity of flycheck in Emacs configs and scripts, which are
so stateful that the deluge of false positives (from the byte-compiler,
package-lint, and checkdoc) can be more overwhelming than helpful.

See `+emacs-lisp-non-package-mode' for details.")

(add-to-list 'auto-mode-alist '("\\.el\\.gz" . emacs-lisp-mode))
(set-eval-handler! '(emacs-lisp-mode lisp-interaction-mode) #'+jg-lisp-eval)

;;-- emacs source paths
(after! (ffap find-func)
  (let ((paths-to-add (append
                       (ffap-all-subdirs "/Volumes/documents/github/_libs/lisp/emacs-src/lisp/" 1)
                       (ffap-all-subdirs (expand-file-name "straight/repos" doom-local-dir) 1)
                       (ffap-all-subdirs (expand-file-name "modules" doom-user-dir))
                       (ffap-all-subdirs (expand-file-name "packages" doom-user-dir))
                       )))
  (mapc (lambda (x)
          (add-to-list 'find-library-source-path x))
        paths-to-add)
  )
  (setq find-function-C-source-directory "/Volumes/documents/github/_libs/lisp/emacs-src/src")

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
(after! jg-completion-templates
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
)
;;-- end file templates

;;-- browse providers
(after! jg-browse-providers
  (pushnew! jg-browse-providers-alist
            '("ELisp Melpa" "https://melpa.org/#/?q=%s")
            '("Elisp Elpa" "https://elpa.gnu.org/packages/")
            )
  )

;;-- end browse providers

;;-- projectile
(after! projectile
  (pushnew! projectile-project-root-files "config.el")
  )
;;-- end projectile
