;;; +xmllint.el -*- lexical-binding: t; -*-

(defun +jg-xml-format-buffer ()
  (interactive)
  (let* ((current (current-buffer))
         (temp-file (make-temp-file "xmllint-temp" nil nil (buffer-string)))
         (result (shell-command-to-string (format jg-xml-format-command-string temp-file)))
         )
    (with-current-buffer current
      (erase-buffer)
      (insert result)
      )
    )
  )

(defun +jg-xml-load-into-repl ()
  (interactive)
  (cond ((process-live-p (get-buffer-process jg-xml-xmllint-shell-buffer-name))
         ;; live, so load
         (with-current-buffer jg-xml-xmllint-shell-buffer-name
           (+jg-text-clear-buffer))
         (comint-simple-send (get-buffer-process jg-xml-xmllint-shell-buffer-name)
                             (format "load %s" (current-buffer)))
         )
        ((buffer-live-p jg-xml-xmllint-shell-buffer-name)
         ;; exists, no process
         (kill-buffer jg-xml-xmllint-shell-buffer-name)
         (+eval/open-repl-other-window)
        )
        (t) ;; don't do anything, as repl will be opened after cond
        )
  (+eval/open-repl-other-window)
  )

(defun +xml/open-repl ()
  " Open an xml lint shell comint on the current buffer / file "
  (interactive)
  (when (get-buffer jg-xml-xmllint-shell-buffer-name)
    (with-current-buffer jg-xml-xmllint-shell-buffer-name
      (erase-buffer)
      )
    )
  (when (process-live-p (get-buffer-process jg-xml-xmllint-shell-buffer-name))
    (kill-process (get-buffer-process jg-xml-xmllint-shell-buffer-name))
    )

  (let* ((fname (buffer-file-name (current-buffer)))
         (is-html (f-ext? fname "html"))
         (default-directory (f-parent fname))
         (comint-buff (apply #'make-comint-in-buffer "xmllint"
                                             jg-xml-xmllint-shell-buffer-name
                                             "xmllint"
                                             nil
                                             (-filter #'identity
                                                      (list (when is-html "--html")
                                                            "--shell"
                                                            fname))))
          )
    (message "Fname: %s" (shell-quote-argument fname))
    (set-process-sentinel (get-buffer-process jg-xml-xmllint-shell-buffer-name)
                          (lambda (process state)
                            (message "Xmllint: %s" state)
                            (when (get-buffer jg-xml-xmllint-shell-buffer-name)
                              (kill-buffer jg-xml-xmllint-shell-buffer-name)
                              )
                            )
                          )
    (pop-to-buffer comint-buff)
    )
  )

(set-repl-handler! 'nxml-mode #'+xml/open-repl)
(set-repl-handler! 'mhtml-mode #'+xml/open-repl)
