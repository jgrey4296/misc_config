(defconst jg_states-packages
  '(
    evil

    )
  )

(defun jg_states/post-init-evil ()
  (evil-define-state test
    "Test State."
    :tag "<T>"
    :message "-- TEST --"
    ;; :enable (motion)
    ;; :input-method t
    :suppress-keymap t
    :entry-hook (test-entry)
    :body
    (setq test-var t)
    (test-hook)
    )

  (evil-add-hjkl-bindings global-map 'test)
  (let ((map (make-sparse-keymap)))
    ;; (evil-local-set-key 'test (kbd "SPC") nil)
    (define-key map (kbd "p") (lambda () (interactive) (message "test")))
    (bind-map map
      ;; :override-minor-modes t
      :evil-keys (dotspacemacs-leader-key)
      :evil-states (test)
      )
    )
  (evil-define-key 'test 'global (kbd "q") (lambda () (interactive) (message "qqqq")))

  ;; (evil-local-set-key 'test (kbd "SPC") nil)
  ;; (evil-local-set-key 'test (kbd "SPC" (lambda () (interactive) (message "aewf")))
  ;; LOOK AT BIND-MAP-DEFAULTS
  (defun test-hook ()
    (message "Blah")
    ;; (define-key evil-test-state-map
    ;;   (kbd dotspacemacs-leader-key) nil)
    )

  (defun test-entry ()
    (message "Test entry")
    )

  )
