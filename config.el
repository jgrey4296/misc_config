;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;-- Me
(defvar user-full-name "John Grey")
(defvar user-mail-address "johngrey4296 at gmail.com")
(defvar user-url "https://jgrey4296.github.io/")
;;-- end Me

;;-- Text Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;; (setq default-input-method "greek")
;;-- end Text Encoding

;;-- bookmarks

(setq bookmark-default-file (pcase system-type
                              ('darwin (expand-file-name "templates/bookmarks/bookmarks.mac" doom-user-dir))
                              ('gnu/linux (expand-file-name "templates/bookmarks/bookmarks.linux" doom-user-dir))
                              )
      )

;;-- end bookmarks

;;-- locations
(setq backup-directory-alist          `((".*" . ,(expand-file-name ".local/backups" doom-emacs-dir)))
      doom-fallback-buffer-name       "base_agenda.org"
      org-directory                   (expand-file-name "~/github/jgrey4296.github.io/orgfiles/")
      initial-buffer-choice           (expand-file-name "base_agenda.org" org-directory)
      org-agenda-files                (list initial-buffer-choice)
      org-archive-location            (string-join `(,(expand-file-name "org/archive.org" doom-user-dir) "* Main Archive") "::")
      pyvenv-default-virtual-env-name (expand-file-name "~/.cache/mamba/envs/")
      server-auth-dir                 (expand-file-name "~/.cache/secrets/emacs")
      native-comp-eln-load-path       (list (expand-file-name "cache/eln" doom-local-dir))
      )

(add-to-list 'load-path (expand-file-name "~/.local/modules"))
;;-- end locations

(setq server-log t)

;;-- keybind clear
(defvar jg-ctl-x-map (make-sparse-keymap))
(setq ctl-x-map jg-ctl-x-map)

;;-- end keybind clear

;;-- doom settings
(setq doom-theme 'jg-Iosvkem)
;; Override doom's whitespace mode settings:
(fset 'doom-highlight-non-default-indentation-h #'(lambda () nil))

(fset 'ad-Advice-newline-and-indent #'(lambda (x &rest _) (funcall x)))
;;-- end doom settings

;;-- global modes
(setq initial-major-mode #'emacs-lisp-mode)

(add-hook! doom-first-buffer
   #'delete-selection-mode
   )

;;-- end global modes

;;-- byte / native compilation
;; from https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(not cl-functions)
      native-comp-async-jobs-number 1
      native-comp-jit-compilation-deny-list '("/with-editor\\.el\\'"
                                              "/vterm\\.el\\'"
                                              "/evil-collection-vterm\\.el\\'"
                                              "/emacs-jupyter.*\\.el\\'"
                                              "/carousel-.*\\.el\\'"
                                              "/lsp-.*\\.el\\'"
                                              )
      )
;;-- end byte / native compilation

;;-- warning suppression
;; (progn
;;   (setq jgtest '(1 2 3)
;;         jgnon  '(blah)
;;         warning-suppress-log-types '((defvaralias losing-value jgblahtest))
;;         warning-suppress-types nil ;;'((defvaralias losing-value jgblahtest))
;;         )

;;   (defvaralias 'jgblahtest 'jgnon)
;;   (defvaralias 'jgblahtest 'jgtest)
;; )

(setq warning-suppress-log-types
      '( ;; Full Suppress
        (defvaralias losing-value woman-topic-history)
        ;; (flycheck syntax-checker)
    ;; ((python python-shell-completion-native-turn-on-maybe))
        ;; (org-element-cache)
        ((flycheck syntax-checker))
        )
      warning-suppress-types
      '( ;; Don't Show
        (defvaralias losing-value python-shell-interpreter)
        ;; ((python python-shell-completion-native-turn-on-maybe))
        ;; (org-element-cache)
        ;; (flycheck syntax-checker)
        )
      )
;;-- end warning suppression
