;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +lsp-signature-stop-maybe-h ()
      "Close the displayed `lsp-signature'."
      (when lsp-signature-mode (lsp-signature-stop) t))
