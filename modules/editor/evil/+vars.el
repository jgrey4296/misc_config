;;; +vars.el -*- lexical-binding: t; -*-

;; General Evil
(setq evil-move-beyond-eol t
      evil-move-cursor-back nil
      evil-mode-line-format 'nil
      evil-symbol-word-search t                    ;; more vim-like behavior
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

;; evil ex
(setq evil-ex-search-vim-style-regexp t
      evil-ex-visual-char-range t                           ;; column range for ex commands
      evil-ex-interactive-search-highlight 'selected-window ;; Only do highlighting in selected window so that Emacs has less work to do highlighting them all.
)

;; evil-snipe
(setq evil-snipe-smart-case t
      evil-snipe-scope        'line
      evil-snipe-repeat-scope 'line
      evil-snipe-char-fold t
)

;;-- evil-escape
(setq evil-escape-inhibit-functions nil ;; '(evil-ex-p)
      evil-escape-excluded-states '(normal multiedit emacs motion)
      evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
      evil-escape-key-sequence "jk"
      evil-escape-delay 0.15
      )

;;-- end evil-escape

;;-- evil-surround and embrace
;; later modified by evil-embrace


(setq-default evil-embrace-show-help-p t
              embrace-show-help-p t
              evil-embrace-evil-surround-keys '(?\( ?\[ ?\{ ?\) ?\] ?\} ?\" ?\' ?< ?> ?b ?B ?t ?\C-\[ ?w ?W ?s ?p ?f ?F)
              evil-surround-pairs-alist (append jg-evil-surround-pairs-base
                                                '((?t . evil-surround-read-tag)
                                                  (?< . evil-surround-read-tag)
                                                  (?f . evil-surround-function)
                                                  (?F . evil-surround-prefix-function))
                                                )
              )
;;-- end evil-surround and embrace

;;-- specs

(spec-handling-add! popup
                    '(evil
                      ("^\\*evil-registers" :size 0.3)
                      ("^\\*Command Line"   :size 8)
                      ("^\\*Ex-Commands\\*" :quit t :select nil :ttl 5)
                      )
                    )

;;-- end specs
