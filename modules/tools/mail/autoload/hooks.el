;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-mail-override-mu4e-hook ()
  (message "Mail override hook: %s" (current-time-string))
  (setq mu4e-use-fancy-chars nil
        mu4e-headers-draft-mark     '("D" . "D")
        mu4e-headers-flagged-mark   '("F" . "F")
        mu4e-headers-new-mark       '("N" . "N")
        mu4e-headers-passed-mark    '("P" . "P")
        mu4e-headers-replied-mark   '("R" . "R")
        mu4e-headers-seen-mark      '("S" . "S")
        mu4e-headers-trashed-mark   '("T" . "T")
        mu4e-headers-attach-mark    '("a" . "a")
        mu4e-headers-encrypted-mark '("x" . "x")
        mu4e-headers-signed-mark    '("s" . "s")
        mu4e-headers-unread-mark    '("u" . "u"))

  (setq auth-source-backend-parser-functions
        '(auth-source-backends-parser-secrets
          auth-source-backends-parser-file))
)
