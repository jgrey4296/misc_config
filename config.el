;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;-- Me

(defvar user-full-name "John Grey")

(defvar user-url "https://jgrey4296.github.io/")
;;-- end Me

;;-- Text Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;; (setq default-input-method "greek")
;;-- end Text Encoding

;;-- locations
(setq backup-directory-alist          `((".*" . ,(expand-file-name ".local/backups" doom-emacs-dir)))
      doom-fallback-buffer-name       "base_agenda.org"
      org-directory                   (expand-file-name "~/github/jgrey4296.github.io/orgfiles/")
      initial-buffer-choice           (expand-file-name "agenda/base_agenda.org" org-directory)
      pyvenv-default-virtual-env-name (expand-file-name "~/_cache_/mamba/envs/")
      server-auth-dir                 (expand-file-name "~/.config/secrets/emacs")
      native-comp-eln-load-path       (list (expand-file-name "cache/eln" doom-local-dir))
      docs-dir                        (expand-file-name "docs" templates-loc)
      )

(add-to-list 'load-path (expand-file-name "~/.local/modules"))

;;-- end locations

;;-- bookmarks
(setq bookmark-default-file (pcase system-type
                              ('darwin (expand-file-name "bookmarks/bookmarks.mac" templates-loc))
                              ('gnu/linux (expand-file-name "bookmarks/bookmarks.linux" templates-loc))
                              )
      )

;;-- end bookmarks

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
;;   (let ((jgtest '(1 2 3))
;;         (jgnon  '(blah))
;;         (warning-suppress-log-types '((defvaralias losing-value jgblahtest)))
;;         (warning-suppress-types nil ;;'((defvaralias losing-value jgblahtest)))
;;         )

;;   (defvaralias 'jgblahtest 'jgnon)
;;   (defvaralias 'jgblahtest 'jgtest)
;; )
;;
(setq warning-suppress-log-types
      '( ;; Full Suppress
        (defvaralias losing-value woman-topic-history)
        (defvaralias losing-value rustic-indent-method-chain)
        ;; (flycheck syntax-checker)
        ;; ((python python-shell-completion-native-turn-on-maybe))
        ((org-element org-element-cache))
        ((flycheck syntax-checker))
        (error "Invalid search bound (wrong side of point)")
        )
      warning-suppress-types
      '( ;; Don't Show, silently added to warnings buffer
        (defvaralias losing-value python-shell-interpreter)
        ;; ((python python-shell-completion-native-turn-on-maybe))
        (org-element org-element-cache)
        (bytecomp)
        ;; (flycheck syntax-checker)
        )
      )
;;-- end warning suppression
