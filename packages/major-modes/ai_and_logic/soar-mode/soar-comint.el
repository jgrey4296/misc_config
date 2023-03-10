;;; soar-mode/soar-comint.el -*- lexical-binding: t; -*-
;; adapted from https://masteringemacs.org/article/comint-writing-command-interpreter
(setq soar-comint-buffer-name "Soar-Comint")
(setq soar-comint-args '())

(setq soar-comint-mode-map
      (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
        (define-key map "\t" 'completion-at-point)
        map))
(setq soar-comint-prompt-regexp "^\\(soar % \\)")

(defun run-soar-comint ()
  " Start up the Soar Command Interpreter "
  (interactive)
  (let* ((prog soar-executable)
         (buffer (comint-check-proc (format "*%s*" soar-comint-buffer-name))))
    (pop-to-buffer-same-window
     (if (or buffer
             (not (derived-mode-p 'soar-comint-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer (format "*%s*" soar-comint-buffer-name)))
       (current-buffer)))
    (unless buffer
      (apply 'make-comint-in-buffer soar-comint-buffer-name buffer prog soar-comint-args)
      (soar-comint-mode))))
(defun soar-comint--initialize ()
  "Helper function to initialize Soar-Comint"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  )
(defun soar-comint-input (x)
  (message "Sending: %s" x))
(defun soar-comint-input-transform (proc x)
  (comint-simple-send proc (format "%s" x)))
(defun soar-comint-preoutput-transform (x)
  (if (string-match soar-comint-prompt-regexp x)
      x
    (format "RECEIVED: %s" x)))
(defun soar-comint-preoutput-store (x)
  (if (not (string-match soar-comint-prompt-regexp x))
      (setq soar-comint-last-received x))
  x
  )
(defun soar-comint-output-response (x)
  (message "Got: %s" x))
(define-derived-mode soar-comint-mode comint-mode "Soar-Comint"
  "Major mode for `run-soar-comint'."
  nil "Soar-Comint"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp soar-comint-prompt-regexp)
  ;; (setq comint-prompt-read-only t)
  ;; (set (make-local-variable 'paragraph-separate) "\\'")
  ;; (set (make-local-variable 'font-lock-defaults) '(soar-comint-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) soar-comint-prompt-regexp)

  ;; (setq-local comint-input-filter-functions '(soar-comint-input))
  ;; (setq-local comint-input-sender 'soar-comint-input-transform)
  (add-hook 'comint-preoutput-filter-functions 'soar-comint-preoutput-transform nil t)
  (add-hook 'comint-preoutput-filter-functions 'soar-comint-preoutput-store nil t)
  ;; (add-hook 'comint-output-filter-functions 'soar-comint-output-response nil t)
  )
;; this has to be done in a hook. grumble grumble.
(add-hook 'soar-comint-mode-hook 'soar-comint--initialize)

(provide 'soar-comint)
