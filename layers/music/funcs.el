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

      (delete-other-windows)
      (display-buffer-same-window tidal-buff nil)
      (select-window (split-window-below))
      (display-buffer-same-window sclang-buff nil)
      (select-window (split-window-right))
      (display-buffer-same-window (get-buffer "*SCLang:PostBuffer*") nil)
      (select-window (get-buffer-window tidal-buff))
      (select-window (split-window-right))
      (display-buffer-same-window (get-buffer "*tidal*") nil)
      )
    )
  )
