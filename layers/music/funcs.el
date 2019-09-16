;; sclang funcs.el
;; loaded third.

(when (and (configuration-layer/package-usedp 'sclang)
           (configuration-layer/package-usedp 'tidal))

  (defun music/start-system ()
    "Startup SCLANG, init appropriate elements,
then start up tidal"
    (interactive)
    (let ((tidal-buff (get-buffer-create music-tidal-workspace))
          (sclang-buff (get-buffer-create music-sclang-workspace)))
      (global-music-mode)
      (with-current-buffer sclang-buff
        (sclang-mode)
        (sclang-start)
        (jg_layer/clear-buffer)
        (insert-file-contents sclang-boot-file)
        )

      (with-current-buffer tidal-buff
        (tidal-mode)
        (tidal-start-haskell)
        )
      (music/setup-windows)
      )
    )

  (defun music/setup-windows ()
    (interactive)
    (delete-other-windows)
    (display-buffer-same-window (get-buffer music-tidal-workspace) nil)
    (select-window (split-window-below))
    (display-buffer-same-window (get-buffer music-sclang-workspace) nil)
    (select-window (split-window-right))
    (display-buffer-same-window (get-buffer "*SCLang:PostBuffer*") nil)
    (select-window (get-buffer-window music-tidal-workspace))
    (select-window (split-window-right))
    (display-buffer-same-window (get-buffer "*tidal*") nil)
    (select-window (get-buffer-window music-tidal-workspace))
    )

  (defun music/setup-minor-mode-keys ()
    (spacemacs/set-leader-keys
      "a . h" 'music-minor/hush
      "a . q" 'music-minor/quit
      "a . w" 'music/setup-windows
      )
    )

  (defun music/clear-minor-mode-keys ()
    (spacemacs/set-leader-keys
      "a . h" nil
      "a . q" nil
      "a . w" nil
      )
    )
)
