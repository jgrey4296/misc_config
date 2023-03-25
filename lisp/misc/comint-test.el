;; adapted from https://masteringemacs.org/article/comint-writing-command-interpreter
(setq my-comint-path "~/.spacemacs.d/lisp/myprog.py")
(setq my-comint-buffer-name "My-Comint")
(setq my-comint-args '())
(setq my-comint-mode-map
      (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
        (define-key map "\t" 'completion-at-point)
        map))
(setq my-comint-prompt-regexp "^\\(:\\)")

(defun run-my-comint ()
  (interactive)
  (let* ((prog my-comint-path)
         (buffer (comint-check-proc (format "*%s*" my-comint-buffer-name))))
    (pop-to-buffer-same-window
     (if (or buffer
             (not (derived-mode-p 'my-comint-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer (format "*%s*" my-comint-buffer-name)))
       (current-buffer)))
    (unless buffer
      (apply 'make-comint-in-buffer my-comint-buffer-name buffer prog my-comint-args)
      (my-comint-mode))))
(defun my-comint--initialize ()
  "Helper function to initialize My-Comint"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  )
(defun my-comint-input (x)
  (message "Sending: %s" x))
(defun my-comint-input-transform (proc x)
  (comint-simple-send proc (format "%s" x)))
(defun my-comint-preoutput-transform (x)
  (if (string-match my-comint-prompt-regexp x)
      x
    (format "RECEIVED: %s" x)))
(defun my-comint-preoutput-store (x)
  (if (not (string-match my-comint-prompt-regexp x))
      (setq my-comint-last-received x))
 x
  )
(defun my-comint-output-response (x)
 (message "Got: %s" x))
(define-derived-mode my-comint-mode comint-mode "My-Comint"
  "Major mode for `run-my-comint'."
  nil "My-Comint"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp my-comint-prompt-regexp)
  ;; (setq comint-prompt-read-only t)
  ;; (set (make-local-variable 'paragraph-separate) "\\'")
  ;; (set (make-local-variable 'font-lock-defaults) '(my-comint-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) my-comint-prompt-regexp)

  ;; (setq-local comint-input-filter-functions '(my-comint-input))
  ;; (setq-local comint-input-sender 'my-comint-input-transform)
  (add-hook 'comint-preoutput-filter-functions 'my-comint-preoutput-transform nil t)
  (add-hook 'comint-preoutput-filter-functions 'my-comint-preoutput-store nil t)
  ;; (add-hook 'comint-output-filter-functions 'my-comint-output-response nil t)
  )
;; this has to be done in a hook. grumble grumble.
(add-hook 'my-comint-mode-hook 'my-comint--initialize)
