;; cmds.el -*- lexical-binding: t; -*-
(require 'lsp-mode)

;;;###autoload
(defun +jg-ide-registered-lsp-clients ()
  (interactive)
  (with-temp-buffer-window "*LSP Clients*" #'popup-window nil
    (princ "**  Registered LSP Clients:  **\n")
    (cl-loop for key being the hash-keys of lsp-clients
             do
             (princ (format "- %s\n" key))
             )
    (princ "\n** Disabled Clients: **\n")
    (dolist (key lsp-disabled-clients)
             (princ (format "- %s\n" key)))
    )
  )

;;;###autoload
(defun +jg-ide-disable-lsp-client ()
  (interactive)
  (ivy-read "Disable Client: "
            (hash-table-keys lsp-clients)
            :action (lambda (x)
                      (add-to-list 'lsp-disabled-clients (intern x)))
            :multi-action (lambda (xs)
                            (dolist (key xs)
                              (add-to-list 'lsp-disabled-clients (intern key))))

            )
  )

;;;###autoload
(defun +jg-ide-enable-lsp-client ()
  (interactive)
  (ivy-read "Enable Client: "
            lsp-disabled-clients
            :action (lambda (x)
                      (setq lsp-disabled-clients (remq (intern x) lsp-disabled-clients)))
            :multi-action (lambda (xs)
                            (setq lsp-disabled-clients
                                  (-reject (-partial #'-contains? (mapcar #'intern xs)) lsp-disabled-clients)))

            )
  )
