;;; config.el -*- lexical-binding: t; -*-

(local-load! "+vars")

(defer-load! jg-bindings-total "+bindings")

(use-package! music-minor-mode
  :commands (music-minor-mode music-minor/music-on global-music-mode)
  :config
  (message "Configuring Jg-Music Minor")
  (map! :map music-minor-mode-map
        ". h" 'music-minor/hush
        ". e" 'music-minor/music-eval-selection
        ". r" 'music-minor/sclang-restart
        ". R" 'music-minor/sclang-recompile
        ". w" '+jg-music/setup-windows
        )
  (add-hook 'music-minor-mode-hook '+jg-music/setup-minor-mode-keys)
  )

(use-package! sclang
  :commands sclang-mode
  :config
  (evil-define-key nil sclang-mode-map
    (kbd "C-c [") nil)
  (evil-define-key '(insert normal) sclang-mode-map
    (kbd "C-c [") nil)
  (evil-define-key '(normal insert) sclang-mode-map
    (kbd "C-c [") '+jg-text-insert-lparen
    (kbd "C-c C-c") 'music-minor/music-eval-line)
  (evil-define-key '(visual) sclang-mode-map
    (kbd "C-c C-c") 'music-minor/music-eval-selection)
  )

(use-package! tidal
  :commands (tidal-mode tidal-start-haskell)
  :config
  (evil-define-key nil tidal-mode-map
    (kbd "C-c C-c") nil)
  (evil-define-key '(normal insert visual) tidal-mode-map
    (kbd "C-c C-c") nil)
  (evil-define-key '(normal insert) tidal-mode-map
    (kbd "C-c C-c") 'music-minor/music-eval-line)
  (evil-define-key '(visual) tidal-mode-map
    (kbd "C-c C-c") 'music-minor/music-eval-selection)
    )

(use-package! chuck-mode
  :commands chuck-mode
  )

(use-package! csound-mode
  :commands csound-mode
  )

(use-package! faustine
  :commands faustine-mode
  :config
  ;; HACK Both `faust-mode' and `faustine-mode' are hardcoded to use
  ;; auto-complete. This silences the obnoxious 'You really should install and
  ;; use auto-complete' warnings when starting them.
  (defvar ac-modes nil)
  (defvar ac-sources nil)

  (map! :localleader
        :map faustine-mode-map
        "RET" #'faustine-mdoc
        "b" #'faustine-build
        "B" #'faustine-build-all
        "c" #'faustine-syntax-check
        "d" #'faustine-diagram
        "D" #'faustine-diagram-all
        "h" #'faustine-online-doc
        "o" #'faustine-toggle-output-buffer
        "s" #'faustine-source-code
        "r" #'faustine-run)
  )
