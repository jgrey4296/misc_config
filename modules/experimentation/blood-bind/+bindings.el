;;; +bindings.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defun jg-test-fn ()
  (interactive)
  (message "testfn")
  )

;; Register bindings
(bloodbind! basic ()
  [ a b c ] :: #'jg-test-fn
  )

(bloodbind! basic ()
  [a b d] :: #'jg-test-fn
  )

;; Compile into keymaps
(blood-bind-compile basic)

;; Apply keymaps to global variables
;; (blood-bind-apply basic)

;;; +bindings.el ends here
