;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +evil-update-cursor-color-h ()
      " Change the cursor color in emacs state. We do it this roundabout way
        to ensure changes in theme doesn't break these colors. "
      (put 'cursor 'evil-emacs-color  (face-foreground 'warning))
      (put 'cursor 'evil-normal-color (face-background 'cursor)))

;;;###autoload
(defun +default-disable-delete-selection-mode-h ()
    (delete-selection-mode -1))

;;;###autoload
(defun +evil-default-cursor-fn ()
  (evil-set-cursor-color (get 'cursor 'evil-normal-color)))

;;;###autoload
(defun +evil-emacs-cursor-fn ()
  (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

;;;###autoload
(defun +evil-disable-ex-highlights-h ()
      "Disable ex search buffer highlights."
      (when (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-nohighlight)
        t))

;;;###autoload
(defun +evil-display-vimlike-save-message-h ()
        "Shorter, vim-esque save messages."
        (message "\"%s\" %dL, %dC written"
                 (if buffer-file-name
                     (file-relative-name (file-truename buffer-file-name) (doom-project-root))
                   (buffer-name))
                 (count-lines (point-min) (point-max))
                 (buffer-size)))

;;;###autoload
(defun +evil-embrace-angle-bracket-modes-hook-h ()
  (when (boundp 'evil-embrace-evil-surround-keys)
    (let ((var (make-local-variable 'evil-embrace-evil-surround-keys)))
      (set var (delq ?< evil-embrace-evil-surround-keys))
      (set var (delq ?> evil-embrace-evil-surround-keys)))
    (embrace-add-pair-regexp ?< "\\_<[a-z0-9-_]+<" ">" #'+evil--embrace-angle-brackets)
    (embrace-add-pair ?> "<" ">")
    ))

;;;###autoload
(defun +evil--escape-exchange-h ()
  (when evil-exchange--overlays
    (evil-exchange-cancel)
    t))

;;;###autoload
(defun +evil--regexp-match-args (arg)
  (when (evil-ex-p)
    (cl-destructuring-bind (&optional arg flags)
        (evil-delimited-arguments arg 2)
      (list arg (string-to-list flags)))))

;;;###autoload
(defun +jg-evil--auto-marks-h ()
  (if (not (or (derived-mode-p ivy-mode helm-mode)
               (s-matches? "^*" (buffer-name))
               ))
      (progn (evil-set-marker ?a (point-min))
             (evil-set-marker ?z (point-max) t))
    nil
    )
  )
