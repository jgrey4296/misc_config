;;; +bindings.el -*- lexical-binding: t; -*-

(defun +jg-nu-command ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (cmd (read-string "Nushell Command: "))
        )
    (call-process "nu" nil (get-buffer-create "*nu*") t "-c" (format cmd (car files)))
    (display-buffer "*nu*")
    )
  )

(spec-handling-add! popup
                    '(nu
                      ("^\\*nu\\*"         :side bottom :ttl 5   :height 0.4 :quit t :select t :priority 50)
                      )
                    )

(map! :map jg-dired-mode-map
      :prefix ("> n" . "nushell")
      :desc "Command" "c" #'+jg-nu-command
      )
