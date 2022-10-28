;;; custom-evil-state.el -*- lexical-binding: t; -*-
;; #+TITLE: Emacs/Evil Keybinding Notes
;; * Keymap Hierarchy:
;; NOTE: See evil-core.el for full design of evil keybindings
;; ** Overriding keymaps/overlay keymaps
;; ** Emulation mode keymaps
;; *** Evil keymaps
;; *** Intercept keymaps
;; *** Local state keymap
;; *** Minor-mode keymaps
;; *** Auxiliary keymaps
;; *** Overriding keymaps
;; *** Global state keymap
;; *** Keymaps for other states
;; ** Minor mode keymaps
;; ** Local keymap (`local-set-key')
;; ** Global keymap (`global-set-key')

(after! (evil bind-map)

  ;; State Creation
  (evil-define-state test
    "Test State."
    :tag "<T>"
    :message "-- TEST --"
    :enable (motion)
    ;; :input-method t
    ;; :entry-hook blah
    ;; :suppress-keymap t
    )

  ;; Global bind
  (global-set-key (kbd "q") (lambda () (interactive) (message "Global q")))
  ;; evil Global state
  (evil-global-set-key 'test (kbd "q") (lambda () (interactive) (message "global state q")))

  ;; Global state leader key binding:
  (let ((map (make-sparse-keymap)))
    ;; Define keybindings:
    ;; (evil-local-set-key 'test (kbd "SPC") nil)
    (define-key map (kbd "p") (lambda () (interactive) (message "test")))

    ;; Add those keybindings behind the leader key in the state:
    (bind-map map
              ;; :override-minor-modes t
              :evil-keys ("SPC")
              :evil-states (test)
              )
    )

  ;; override state
  (let* ((map (make-sparse-keymap)))
    (evil-make-overriding-map map 'test)
    (define-key map (kbd "q") (lambda () (interactive) (message "override q")))
    (defvar my-override-mode-map map)

    (define-minor-mode my-override-mode "global override" :global t)
    (my-override-mode)
    )
  ;; auxiliary
  (evil-define-key 'test 'evil-test-state-map (kbd "q") (lambda () (interactive) (message "auxiliary q")))

  ;; minor mode state
  ;; evil-define-minor-mode-key

  ;; local state
  (evil-local-set-key 'test (kbd "q") (lambda () (interactive) (message "local state q")))

  ;; intercept
  (let* ((map (make-sparse-keymap)))
    (evil-make-intercept-map map 'test)
    (define-key map (kbd "q") (lambda () (interactive) (message "intercept q")))
    (defvar my-intercept-mode-map map)

    (define-minor-mode my-intercept-mode "global intercept" :global t)
    (my-intercept-mode)
    )

  ;; ----------------------------------
  (evil-add-hjkl-bindings global-map 'test)

  ;; Add keybindings directly, without leader key:
  ;; (evil-define-key 'test 'global (kbd "q") (lambda () (interactive) (message "qqqq")))
  ;; (evil-local-set-key 'test (kbd "SPC" (lambda () (interactive) (message "aewf")))

  ;; LOOK AT BIND-MAP-DEFAULTS

  )
