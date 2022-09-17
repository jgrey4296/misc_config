;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;-- Me
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Grey"
      user-mail-address "johngrey4296 at gmail.com")
;;-- end Me

;;-- Text Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq default-input-method "greek")
;;-- end Text Encoding

;;-- locations
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
      ispell-personal-dictionary (expand-file-name "~/.ispell_english")
      pyvenv-default-virtual-env-name "~/anaconda3/envs/"
      org-directory "~/github/writing/orgfiles/"
      org-agenda-files `(,(expand-file-name "~/.doom.d/setup_files/base_agenda.org"))
      org-archive-location (string-join `(,(expand-file-name "~/.doom.d/setup_files/archive.org")
                                          "* Main Archive") "::")
      initial-buffer-choice "/Volumes/documents/github/emacs_files/setup_files/base_agenda.org"
      doom-fallback-buffer-name "base_agenda.org"
      bookmark-default-file "/Volumes/documents/github/emacs_files/bookmarks"
      )
;;-- end locations

;;-- evil
(setq evil-collection-setup-minibuffer t
      evil-move-beyond-eol t
      evil-move-cursor-back nil
      evil-snipe-repeat-scope nil
 )
;;-- end evil

;;-- doom settings
(setq +doom-quit-messages nil
      doom-theme 'doom-Iosvkem)
;; Override doom's whitespace mode settings:
(fset 'doom-highlight-non-default-indentation-h #'(lambda () nil))
;;-- end doom settings

;;-- which key
(setq which-key-idle-secondary-delay 0.05
      which-key-sort-order 'which-key-key-order-alpha
 )
;;-- end which key

;;-- global modes
(setq smartparens-global-mode nil
      flycheck-global-modes nil
      )

(remove-hook! doom-first-buffer
  #'smartparens-global-mode
  )
(remove-hook! 'after-change-major-mode-hook
  #'+ligatures-init-buffer-h
  #'global-flycheck-mode-enable-in-buffers
  )
(remove-hook! 'doom-init-ui-hook
  #'+ligatures-init-h
  )
;;-- end global modes

;;-- misc variables
(setq
 +lsp-defer-shutdown 10
 avy-all-windows t
 display-line-numbers-type t
 display-line-numbers-width 4
 highlight-indent-guides-suppress-auto-error t
 ibuffer-old-time 2
 line-move-ignore-invisible t
 outline-blank-line nil
 overflow-newline-into-fringe t
 tab-always-indent t
 whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)
 )
;;-- end misc variables


;;-- Byte Compilation
;; from https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(not cl-functions))
;;-- end Byte Compilation
