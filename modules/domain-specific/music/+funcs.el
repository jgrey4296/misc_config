;; sclang funcs.el
;; loaded third.

(when (and (featurep! sclang)
           (featurep! tidal))

  (defun jg-music-layer/start-system ()
    "Startup SCLANG, init appropriate elements,
then start up tidal"
    (interactive)
    (let ((tidal-buff (get-buffer-create jg-music-layer-tidal-workspace))
          (sclang-buff (get-buffer-create jg-music-layer-sclang-workspace)))
      (global-jg-music-layer-mode)
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
      (jg-music-layer/setup-windows)
      )
    )
  (defun jg-music-layer/setup-windows ()
    (interactive)
    (delete-other-windows)
    (display-buffer-same-window (get-buffer jg-music-layer-tidal-workspace) nil)
    (select-window (split-window-below))
    (display-buffer-same-window (get-buffer jg-music-layer-sclang-workspace) nil)
    (select-window (split-window-right))
    (display-buffer-same-window (get-buffer "*SCLang:PostBuffer*") nil)
    (select-window (get-buffer-window jg-music-layer-tidal-workspace))
    (select-window (split-window-right))
    (display-buffer-same-window (get-buffer "*tidal*") nil)
    (select-window (get-buffer-window jg-music-layer-tidal-workspace))
    )
  (defun jg-music-layer/setup-minor-mode-keys ()
    (map! :leader
      "a . h" 'jg-music-layer-minor/hush
      "a . q" 'jg-music-layer-minor/quit
      "a . w" 'jg-music-layer/setup-windows
      )
    )
  (defun jg-music-layer/clear-minor-mode-keys ()
    (map! :leader
      "a . h" nil
      "a . q" nil
      "a . w" nil
      )
    )
)
