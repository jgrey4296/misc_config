

(defvar jgtest-var nil)

(transient-define-argument jg-test-switch ()
  :class 'transient-switches
  :argument-format "%s"
  :argument-regexp "%s"
  :choices '("a" "b" "c")
  )

(transient-define-suffix jgtest ()
  :transient nil
  :key "t"
  :format "%k -> %d"
  :description (lambda () (format "[%s]" jgtest-var))
  (interactive)
  (message "%s %s"
           (transient-args!)
           ;; (transient-arg-value "-a=" (transient-args transient-current-command))
           (transient-args? '-a=b)
           )
  (setq jgtest-var (not jgtest-var))
)

(transient-define-suffix jgtest-buffer (buffer-name)
  "An interactive suffix that obtains a buffer name from the user."
  (interactive "b")
  :transient t
  (message "You selected: %s" buffer-name)
  )

(transient-define-prefix jgtest-nested ()
  [ ("n" jgtest) ]
  )

(transient-define-prefix jgtest-transient ()
  "a test transient"
  ["column"
   :class transient-row
   ("a" "Switch" jg-test-switch :transient t :allow-empty nil :always-read t :init-value ,(transient-init! "b"))
   ("b" "buffer" jgtest-buffer :transient t)
   ("c" "nest" jgtest-nested)
   ("d" jgtest :transient t)
   ]
  [2 "row"
   :class transient-row
   :pad-keys nil
   ("-s" "inline switch" "inline" :init-value ,(transient-init! "inline")) ;; (lambda (obj) (oset obj value "inline")))
   ("-c" "choices" "--choice=" :choices (foo bar baz))
   ("--d" "other" ("other" "--doo"))
   ]
  [
   "Second Group"
   ("q" "Quit" transient-quit-one)
   ]
  )
