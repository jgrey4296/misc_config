;;; +vars.el -*- lexical-binding: t; -*-

(setq evil-collection-setup-minibuffer t
      evil-move-beyond-eol t
      evil-move-cursor-back nil
      evil-snipe-repeat-scope nil

      save-silently (not noninteractive)
      evil-ex-search-vim-style-regexp t
      evil-ex-visual-char-range t  ;; column range for ex commands
      evil-mode-line-format 'nil
      evil-symbol-word-search t      ;; more vim-like behavior
      evil-default-cursor '+evil-default-cursor-fn ;; if the current state is obvious from the cursor's color/shape, then we won't need superfluous indicators to do it instead.
      evil-normal-state-cursor 'box
      evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
      evil-insert-state-cursor 'bar
      evil-visual-state-cursor 'hollow
      evil-ex-interactive-search-highlight 'selected-window ;; Only do highlighting in selected window so that Emacs has less work to do highlighting them all.
      ;; It's infuriating that innocuous "beginning of line" or "end of line"
      ;; errors will abort macros, so suppress them:
      evil-kbd-macro-suppress-motion-error t
      evil-undo-system (cond ((modulep! :emacs undo +tree) 'undo-tree)
                             ((modulep! :emacs undo) 'undo-fu)
                             ((> emacs-major-version 27) 'undo-redo))

      evil-embrace-show-help-p nil
      evil-escape-excluded-states '(normal visual multiedit emacs motion)
      evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
      evil-escape-key-sequence "jk"
      evil-escape-delay 0.15

      evil-snipe-smart-case t
      evil-snipe-scope 'line
      evil-snipe-repeat-scope 'visible
      evil-snipe-char-fold t
      evil-textobj-anyblock-blocks '(("(" . ")")
                                     ("{" . "}")
                                     ("\\[" . "\\]")
                                     ("<" . ">"))
      )
;;-- specs

(spec-handling-add! popup nil
                    '(evil
                      ("^\\*evil-registers" :size 0.3)
                      ("^\\*Command Line"   :size 8)
                      )
                    )

;;-- end specs