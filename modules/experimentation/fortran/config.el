;;; lang/fortran/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(local-load! "+vars")

(use-package! f90
  :defer t
  :config
  ;; --- Compilation --- ;;
  ;; Used by `compile' (SPC c c)
  (let ((cmd (cond ((modulep! +intel) "ifort ")
                   (t "gfortran "))))
    (setq-hook! 'f90-mode-hook
      compile-command cmd
      compilation-buffer-name-function #'+fortran-compilation-buffer-name-fn))

  ;; --- LSP Configuration --- ;;
  (setq lsp-clients-fortls-args '("--enable_code_actions" "--hover_signature"))

  ;; --- Keybindings --- ;;
  (map! :map f90-mode-map
        :localleader
        (:prefix ("f" . "fpm")
         :desc "fpm build" "b" #'+fortran/fpm-build
         :desc "fpm run"   "r" #'+fortran/fpm-run
         :desc "fpm test"  "t" #'+fortran/fpm-test)
        (:prefix ("g" . "gfortran")
         :desc "compile" "c" #'+fortran/gfortran-compile
         :desc "run"     "r" #'+fortran/gfortran-run)
        :desc "build" "b" #'+fortran/build
        :desc "run"   "r" #'+fortran/run)

  (when (modulep! +intel)
    (map! :map f90-mode-map
          :localleader
          (:prefix ("i" . "ifort")
           :desc "compile" "c" #'+fortran/ifort-compile
           :desc "run"     "r" #'+fortran/ifort-run)))

  )

(use-package! fortran
  :commands fortran-mode
  ;; The `.for' extension is automatically recognized by Emacs and invokes
  ;; `fortran-mode', but not its capital variant `.FOR'. Many old files are
  ;; named the latter way, so we account for that manually here.
  :config
  ;; Or else Flycheck will get very mad.
  (setq flycheck-gfortran-language-standard "legacy")

  ;; --- Compilation --- ;;
  ;; Used by `compile' (SPC c c)
  (let ((cmd (cond ((modulep! +intel) "ifort ")
                   (t "gfortran -std=legacy "))))
    (setq-hook! 'fortran-mode-hook
      compile-command cmd
      compilation-buffer-name-function #'+fortran-compilation-buffer-name-fn))

  ;; --- Keybindings --- ;;
  (map! :map fortran-mode-map
        :localleader
        (:prefix ("g" . "gfortran")
         :desc "compile" "c" #'+fortran/gfortran-compile
         :desc "run"     "r" #'+fortran/gfortran-run))

  (when (modulep! +intel)
    (map! :map fortran-mode-map
          :localleader
          (:prefix ("i" . "ifort")
           :desc "compile" "c" #'+fortran/ifort-compile
           :desc "run"     "r" #'+fortran/ifort-run)))
  )
