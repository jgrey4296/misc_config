;; config.el<4> -*- mode: elisp; lexical-binding: t; -*-

;; Use spotlight search backend as a default for M-x locate (and helm/ivy
;; variants thereof), since it requires no additional setup.
(setq locate-command "mdfind")


(when (eq system-type 'darwin)
  (pushnew! auth-sources 'macos-keychain-internet 'macos-keychain-generic)
  )

(use-package! osx-trash
  :commands osx-trash-move-file-to-trash
  :init
  ;; Delete files to trash on macOS, as an extra layer of precaution against
  ;; accidentally deleting wanted files.
  (setq delete-by-moving-to-trash t)

  ;; Lazy load `osx-trash'
  (when (not (fboundp 'system-move-file-to-trash))
    (defun system-move-file-to-trash (file)
      "Move FILE to trash."
      (when (and (not IS-LINUX)
                 (not (file-remote-p default-directory)))
        (osx-trash-move-file-to-trash file)))))
