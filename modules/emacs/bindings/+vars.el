;;; util/bindings/+vars.el -*- lexical-binding: t; -*-

(setq-default jg-google-url "https://duckduckgo.com/?q=%s"
              jg-twitter-url "https://twitter.com"
              default-input-method "greek"

              jg-misc-ibuffer-heuristics (rx (or "backtab"
                                                 (regexp "\\.\\.")
                                                 (regexp "^[[:alpha:]]\\{2,\\}")
                                                 (regexp "which-key")
                                                 (regexp "/ S")
                                                 )
                                             )
              )


(setq jg-binding-operator-map           (make-sparse-keymap "evil operators")
      jg-binding-vision-map             (make-sparse-keymap "vision manipulation")
      jg-binding-forward-motion-map     (make-sparse-keymap "forward motion")
      jg-binding-backward-motion-map    (make-sparse-keymap "backward motion")
      jg-binding-inner-text-objects-map (make-sparse-keymap "inner textobjs")
      jg-binding-outer-text-objects-map (make-sparse-keymap "outer textobjs")

      jg-binding-normal-state-map (make-sparse-keymap "jg-binding-normal-state-map")
      jg-binding-visual-state-map (make-sparse-keymap "jg-binding-visual-state-map")
      jg-binding-operator-state-map (make-sparse-keymap "jg-binding-operator-state-map")
      jg-binding-motion-state-map (make-sparse-keymap "jg-binding-motion-state-map")
      )

(after! evil
  (setq old-evil-normal-state-map evil-normal-state-map
        old-evil-visual-state-map evil-visual-state-map
        old-evil-operator-state-map evil-operator-state-map
        old-evil-motion-state-map evil-motion-state-map

        evil-normal-state-map jg-binding-normal-state-map
        evil-visual-state-map jg-binding-visual-state-map
        evil-operator-state-map jg-binding-operator-state-map
        evil-motion-state-map jg-binding-motion-state-map
        )


  (setq evil-global-keymaps-alist
        '((evil-emacs-state-minor-mode    . evil-emacs-state-map)
          (evil-motion-state-minor-mode   . evil-motion-state-map)
          (evil-replace-state-minor-mode  . evil-replace-state-map)
          (evil-operator-state-minor-mode . evil-operator-state-map)
          (evil-visual-state-minor-mode   . evil-visual-state-map)
          (evil-insert-state-minor-mode   . evil-insert-state-map)
          (evil-normal-state-minor-mode   . evil-normal-state-map)))
)
