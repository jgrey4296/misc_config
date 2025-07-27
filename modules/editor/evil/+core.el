;;; +core.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;-- advice
(advice-add 'help-with-tutorial   :after #'(lambda (&rest _) (evil-emacs-state +1)))
(advice-add 'evil-indent          :around #'+evil--dont-move-cursor-a)
(advice-add 'evil-global-marker-p :after-until #'+evil--make-numbered-markers-global-a)
(advice-add 'turn-on-evil-mode    :before #'+evil--fix-local-vars-a)
(advice-add 'helpful-key          :before #'+evil--fix-helpful-key-in-evil-ex-a)

;; Make J (evil-join) remove comment delimiters when joining lines.
(advice-add 'evil-join          :around #'+evil-join-a)
(advice-add 'evil-fill          :around #'+evil--no-squeeze-on-fill-a)
(advice-add 'evil-fill-and-move :around #'+evil--no-squeeze-on-fill-a)
;; monkey patch `evil-ex-replace-special-filenames' to improve support for
;; file modifiers like %:p:h. This adds support for most of vim's modifiers,
;; and one custom one: %:P (expand to the project root).
(advice-add 'evil-ex-replace-special-filenames :override #'+evil-replace-filename-modifiers-a)
;; make `try-expand-dabbrev' (from `hippie-expand') work in minibuffer
(add-hook 'minibuffer-inactive-mode-hook #'+evil--fix-dabbrev-in-minibuffer-h)
;; Focus and recenter new splits
(advice-add 'evil-window-split  :override #'+evil-window-split-a)
(advice-add 'evil-window-vsplit :override #'+evil-window-vsplit-a)
;; Make o/O continue comments (see `+evil-want-o/O-to-continue-comments' to disable)
(advice-add 'evil-open-above              :around #'+evil--insert-newline-above-and-respect-comments-a)
(advice-add 'evil-open-below              :around #'+evil--insert-newline-below-and-respect-comments-a)
(advice-add 'iedit-show-all               :override #'+jg-evil-iedit-show-all)
(advice-add 'counsel-mark--get-candidates :filter-args #'+jg-evil-marks-cleanup)
(advice-add 'evil-find-char               :override #'+jg-evil-find-char)

;;-- end advice

;;-- early defs
;; Set these defaults before `evil'; use `defvar' so they can be changed prior
;; to loading.
(defvar evil-want-C-g-bindings t)
(defvar evil-want-C-i-jump nil)  ; we do this ourselves
(defvar evil-want-C-u-scroll t)  ; moved the universal arg to <leader> u
(defvar evil-want-C-u-delete t)
(defvar evil-want-C-w-delete t)
(defvar evil-want-Y-yank-to-eol t)
(defvar evil-want-abbrev-expand-on-insert-exit nil)
(defvar evil-respect-visual-line-mode nil)

;;-- end early defs

(use-package! evil
  :hook (doom-after-modules-config . evil-mode)
  :demand t
  :preface
  ;; Slow this down from 0.02 to prevent blocking in large or folded buffers
  ;; like magit while incrementally highlighting matches.
  (setq-hook! '(magit-mode-hook so-long-minor-mode-hook)
    evil-ex-hl-update-delay 0.25)

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; Forward declare these so that ex completion works, even if the autoloaded
  ;; functions aren't loaded yet.
  (evil-add-command-properties '+evil:align :ex-arg 'regexp-match)
  (evil-add-command-properties '+evil:align-right :ex-arg 'regexp-match)
  (evil-add-command-properties '+multiple-cursors:evil-mc :ex-arg 'regexp-global-match)

  ;; Allow eldoc to trigger directly after changing states
  (after! eldoc (eldoc-add-command 'evil-normal-state 'evil-insert 'evil-change 'evil-delete 'evil-replace))
  (unless noninteractive (add-hook! 'after-save-hook #'+evil-display-vimlike-save-message-h))

  ;; stop clipboard clobbering in visual state
  (setq evil-visual-update-x-selection-p nil)


  ;;-- hooks
  (add-hook 'doom-load-theme-hook           #'+evil-update-cursor-color-h)
  (add-hook 'doom-after-modules-config-hook #'+evil-update-cursor-color-h)
  (add-hook 'evil-insert-state-entry-hook   #'delete-selection-mode)
  (add-hook 'evil-insert-state-exit-hook    #'+default-disable-delete-selection-mode-h)
  ;; (add-hook 'evil-local-mode-hook           #'+jg-evil--auto-marks-h)

  ;;-- end hooks

  ;; Lazy load evil ex commands
  ;; (delq! 'evil-ex features)
  ;; (add-transient-hook! 'evil-ex (provide 'evil-ex))
  )

;; --------------------------------------------------

(setq evil-move-beyond-eol t
      evil-move-cursor-back nil
      evil-mode-line-format 'nil
      evil-symbol-word-search t ;; more vim-like behavior
      evil-kbd-macro-suppress-motion-error t ;; stop beg/end of line errors aborting macros

      evil-default-cursor      '+evil-default-cursor-fn ;; if the current state is obvious from the cursor's color/shape, then we won't need superfluous indicators to do it instead.
      evil-normal-state-cursor 'box
      evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
      evil-insert-state-cursor 'bar
      evil-visual-state-cursor 'hollow


      evil-undo-system (cond ((modulep! :emacs undo +tree) 'undo-tree)
                             ((modulep! :emacs undo) 'undo-fu)
                             ((> emacs-major-version 27) 'undo-redo))
      )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 27, 2025
;; Modified:   July 27, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; +core.el ends here
