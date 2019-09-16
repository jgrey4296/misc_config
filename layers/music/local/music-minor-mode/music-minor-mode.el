(defun music-minor/music-eval-selection ()
  """ Evaluate a selected region, or the entire buffer, in tidal OR SCLang """
  (interactive)
  (let* ((start (if (eq evil-state 'visual) evil-visual-beginning (point-min)))
         (last (if (eq evil-state 'visual) evil-visual-end  (point-max)))
         (str (buffer-substring-no-properties start last)))
    (if (eq major-mode 'tidal-mode)
        (tidal-send-string str)
      (sclang-eval-string str t)
      )
    )
  )

(defun music-minor/music-eval-line ()
  (interactive)
  (let* ((str (buffer-substring (line-beginning-position)
                                (line-end-position))))
    (if (eq major-mode 'tidal-mode)
        (progn (tidal-send-string str)
               (with-current-buffer tidal-buffer
                 (comint-send-input)))
      (sclang-eval-string str t)
      )
    (pulse-momentary-highlight-one-line (point))
    )
  )

(defun music-minor/hush ()
  (interactive)
  (tidal-send-string "hush")
  (sclang-eval-string "thisProcess.stop" t)
  )

(defun music-minor/sclang-restart ()
  (interactive)
  (sclang-server-reboot)
  )

(defun music-minor/sclang-recompile ()
  (interactive)
  (sclang-recompile)
  )

(defun music-minor/quit ()
  (interactive)
  (if (get-buffer "*tidal*")
      (progn (message "Shutting down Tidal")
             (kill-buffer "*tidal*")))
  (if (get-buffer "*SCLang:PostBuffer*")
      (progn (message "Shutting down SCLang")
             (kill-buffer "*SCLang:PostBuffer*")))
  (global-music-mode 0)

  (if (functionp 'music/clear-minor-mode-keys)
      (music/clear-minor-mode-keys))
  )

(define-minor-mode music-minor-mode
  "A Minor mode for generalised SCLang/Tidal/Chuck commands"
  :lighter "Music"
  )

(defun music-on ()
  (unless (minibufferp)
    (if (or (eq major-mode 'tidal-mode ) (eq major-mode 'sclang-mode))
        (music-minor-mode 1))
    )
  )

(define-globalized-minor-mode global-music-mode music-minor-mode music-on)

(provide 'music-minor-mode)
