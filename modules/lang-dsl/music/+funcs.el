;; sclang funcs.el
;; loaded third.

(defun +jg-music/start-system ()
    "Startup SCLANG, init appropriate elements,
then start up tidal"
    (interactive)
    (let ((tidal-buff (get-buffer-create jg-music-tidal-workspace))
          (sclang-buff (get-buffer-create jg-music-sclang-workspace)))
      (global-jg-music-layer-mode)
      (with-current-buffer sclang-buff
        (sclang-mode)
        (sclang-start)
        (+jg-text-clear-buffer)
        (insert-file-contents sclang-boot-file)
        )

      (with-current-buffer tidal-buff
        (tidal-mode)
        (tidal-start-haskell)
        )
      (+jg-music/setup-windows)
      )
    )
(defun +jg-music/setup-windows ()
    (interactive)
    (delete-other-windows)
    (display-buffer-same-window (get-buffer jg-music-tidal-workspace) nil)
    (select-window (split-window-below))
    (display-buffer-same-window (get-buffer jg-music-sclang-workspace) nil)
    (select-window (split-window-right))
    (display-buffer-same-window (get-buffer "*SCLang:PostBuffer*") nil)
    (select-window (get-buffer-window jg-music-tidal-workspace))
    (select-window (split-window-right))
    (display-buffer-same-window (get-buffer "*tidal*") nil)
    (select-window (get-buffer-window jg-music-tidal-workspace))
    )
(defun +jg-music/setup-minor-mode-keys ()
  (map! :leader
        "a . h" 'music-minor/hush
        "a . q" 'music-minor/quit
        "a . w" '+jg-music/setup-windows
        )
  )
(defun +jg-music/clear-minor-mode-keys ()
  (map! :leader
        "a . h" nil
        "a . q" nil
        "a . w" nil
        )
  )
