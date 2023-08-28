;;; +modes.el -*- lexical-binding: t; -*-

;;
;; Global modes

(defvar +popup-mode-map (make-sparse-keymap)
  "Active keymap in a session with the popup system enabled. See
`+popup-mode'.")

(defvar +popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (when (modulep! :editor evil)
      ;; For maximum escape coverage in emacs state buffers; this only works in
      ;; GUI Emacs, in tty Emacs use C-g instead
      (define-key map [escape] #'doom/escape))
    map)
  "Active keymap in popup windows. See `+popup-buffer-mode'.")

(define-minor-mode +popup-mode
  "Global minor mode representing Doom's popup management system."
  :init-value nil
  :global t
  :keymap +popup-mode-map
  (cond (+popup-mode
         (add-hook 'doom-escape-hook #'+popup-close-on-escape-h 'append)
         (setq window--sides-inhibit-check t)
         (dolist (prop +popup-window-parameters)
           (push (cons prop 'writable) window-persistent-parameters)))
        (t
         (remove-hook 'doom-escape-hook #'+popup-close-on-escape-h)
         (setq display-buffer-alist +popup--old-display-buffer-alist
               window--sides-inhibit-check nil)
         (dolist (prop +popup-window-parameters)
           (delq (assq prop window-persistent-parameters)
                 window-persistent-parameters)))))

(define-minor-mode +popup-buffer-mode
  "Minor mode for individual popup windows.

It is enabled when a buffer is displayed in a popup window and disabled when
that window has been changed or closed."
  :init-value nil
  :keymap +popup-buffer-mode-map
  (if (not +popup-buffer-mode)
      (remove-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h t)
    (add-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h
              nil 'local)
    (when (timerp +popup--timer)
      (remove-hook 'kill-buffer-hook #'+popup-kill-buffer-hook-h t)
      (cancel-timer +popup--timer)
      (setq +popup--timer nil))))

(put '+popup-buffer-mode 'permanent-local t)
(put '+popup-buffer-mode 'permanent-local-hook t)
(put '+popup-set-modeline-on-enable-h 'permanent-local-hook t)
