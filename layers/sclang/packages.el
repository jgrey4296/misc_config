;; sclang packages.el
;; loads second

(defconst sclang-packages
  '(
    (scel :location local)
    ;; package from EPA
    ;; eg: some-package
    ;; (some-package :location elpa)
    ;; (some-package :location local)
    ;; (some-package :location (recipe :fetcher github :repo "some/repo"))
    ;;(some-package :excluded t)
    )

  )

;; (defun <layer>/pre-init-<package>)
;; (defun <layer>/init-<package>)
;; (defun <layer>/post-init-<package>)

(defun sclang/init-scel ()
  (use-package scel
    :commands (sclang-mode)
    )
  )

(defun sclang/post-init-scel ()
;;sclang variables:
(setq-default sclang-auto-scroll-post-buffer t
              sclang-help-path (quote ("/Applications/SuperCollider/Help"))
              sclang-program "sclang"
              sclang-rtf-editor-program "emacs"
              sclang-show-workspace-on-startup nil
              show-paren-mode t
              sclang-library-configuration-file (expand-file-name "~/.spacemacs.d/layers/sclang/sclang.yaml")
              sclang-udp-port 57120
              sclang-eval-line-forward nil
              sclang-runtime-directory (expand-file-name "~/.sclang/")
              )
  )
