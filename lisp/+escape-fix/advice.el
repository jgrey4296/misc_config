;;; +evil-escape.el -*- lexical-binding: t; -*-

(setq jgtestvar 1)

(defun +jg-self-insert ()
  (message "Inserted: %c %s" last-command-event evil-state)
  )
(add-hook! 'post-self-insert-hook #'+jg-self-insert)
(remove-hook! 'post-self-insert-hook #'+jg-self-insert)

(string-to-char "j")

(defun jg-evil-ex-echo (string &rest args)
  nil)

(advice-add 'evil-ex-echo :override #'jg-evil-ex-echo)
(advice-remove 'evil-ex-echo #'jg-evil-ex-echo)

(setq evil-ex-hl-update-delay 1)

(evil-define-command jg-ex (&optional initial-input)
  :keep-visual t
  :repeat abort
  (interactive)
  (let ((initial-input "`<,`>")
        (evil-ex-current-buffer (current-buffer))
        (evil-ex-previous-command "ls")
        evil-ex-argument-handler result
        )
  (minibuffer-with-setup-hook
      (lambda ()
        (evil-ex-setup)
        (evil-ex-update)
        )
    (message "Read: %s"
             (read-from-minibuffer ":" initial-input evil-ex-completion-map nil
                                   'evil-ex-history nil t))))
)

(jg-ex)

(setq minibuffer-setup-hook nil)
;; (add-hook! 'minibuffer-setup-hook #'evil-collection-minibuffer-insert)
(add-hook! 'minibuffer-setup-hook #'jg-mini)
(add-hook! 'minibuffer-setup-hook #'savehist-minibuffer-hook)
(add-hook! 'minibuffer-setup-hook #'winner-save-unconditionally)
(add-hook! 'minibuffer-setup-hook #'doom-init-smartparens-in-minibuffer-maybe-h)
(add-hook! 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook! 'minibuffer-setup-hook #'rfn-eshadow-setup-minibuffer)
(add-hook! 'minibuffer-setup-hook #'minibuffer-error-initialize)
(add-hook! 'minibuffer-setup-hook #'minibuffer-history-isearch-setup)
(add-hook! 'minibuffer-setup-hook #'minibuffer-history-initialize)

(defun jg-mini ()
  ;; (set (make-local-variable 'evil-echo-state) nil)
  ;; (evil-set-initial-state 'mode 'insert) is the evil-proper
  ;; way to do this, but the minibuffer doesn't have a mode.
  ;; The alternative is to create a minibuffer mode (here), but
  ;; then it may conflict with other packages' if they do the same.
  (evil-insert 1))


(define-advice evil-ex-call-command (:around (fn &rest args) +jg-log )
  (message "Initial: - %s\n - %s\n - %s" real-this-command this-command this-original-command)
  (apply fn args)
  (message "Initial: - %s\n - %s\n - %s" real-this-command this-command this-original-command)
  )
(advice-remove 'evil-ex-call-command 'evil-ex-call-command@+jg-log)

(define-advice next-line (:before (&rest args) +jg-log)
  (backtrace)
  )

(define-advice evil-ex-execute (:before (&rest args) +jg-log)
  (message "Executing: %s" args)
  )

(defun +jg-test-msg ()
  (interactive)
  (message "blah")
  )

(setq evil-ex-completion-map (make-sparse-keymap))
(map! :map evil-ex-completion-map
      :i "i" #'+jg-test-msg
      )
(suppress-keymap evil-ex-completion-map)

(define-advice evil-ex (:around (fn &rest args) +jg-log)
  (condition-case err
      (apply fn args)
    ((wrong-type-argument
      (debug)))))

(advice-remove 'evil-ex 'evil-ex@+jg-log)
(advice-remove 'next-line 'next-line@+jg-log)

(setq default-input-method nil)

(define-advice evil-next-line (:before (&rest args) +jg-log)
  (backtrace)
  )
(advice-remove 'evil-next-line 'evil-next-line@+jg-log)


(setq post-command-hook nil)
      (highlight-parentheses--initiate-highlight
       company-post-command
       t)

(define-advice evil-ex-setup (:before (&rest args) +jg-log)
  (message "Setting up")
  )

(define-advice evil-ex-teardown (:before (&rest args) +jg-log)
  (message "Tearing Down")
  )

(defun jg-pre-cmd-log ()
  (message "Pre commands: %s" pre-command-hook)
  )

(add-hook! 'pre-command-hook 'jg-pre-cmd-log :depth -200)

(remove-hook! 'pre-command-hook 'jg-pre-cmd-log)

(define-advice evil-range (:around (fn &rest args) +jg-log)
  (let (val)
  (message "Evil Range before: %s %s" real-this-command this-command)
  (setq val (apply fn args))
  (message "Evil Range after: %s %s" real-this-command this-command)
  val
  )
  )
(advice-remove 'evil-range 'evil-range@+jg-log)

(define-advice evil-line-position (:around (fn &rest args) +jg-log)
  (let (val)
  (message "Evil line-pos before: %s %s" real-this-command this-command)
  (setq val (apply fn args))
  (message "Evil line-pos after: %s %s" real-this-command this-command)
  val
  )
  )
(advice-remove 'evil-line-position 'evil-line-position@+jg-log)

(defun evil-line-position (line &optional column)
  "Return the position of LINE.
If COLUMN is specified, return its position on the line.
A negative number means the end of the line."
  (declare-function evil-goto-line "evil-commands")
  (save-excursion
    (evil-goto-line line)
    (if (numberp column)
        (if (< column 0)
            (beginning-of-line 2)
          (move-to-column column))
      (beginning-of-line))
    (point)))


(define-advice evil-goto-line (:around (fn &rest args) +jg-log)
  (let (val)
  (message "Evil goto before: %s %s" real-this-command this-command)
  (setq val (apply fn args))
  (message "Evil goto  after: %s %s" real-this-command this-command)
  val
  )
  )
(advice-remove 'evil--goto-line 'evil--goto-line@+jg-log)
(advice-remove 'evil-range 'evil-range@+jg-log)

(defun jg-log-post ()
  (message "----- post command: %s %s %c" real-this-command this-command last-command-event)
  )

(defun jg-log-pre()
  (message "----- pre command: %s %s %c" real-this-command this-command last-command-event)
  )
(add-hook! 'post-command-hook 'jg-log-post)
(add-hook! 'pre-command-hook 'jg-log-pre)

(defun jg-log-insert ()
  (message "-- post self insert: %s %s %c" real-this-command this-command last-command-event)
  )
(add-hook! 'post-self-insert-hook 'jg-log-insert)

(define-advice next-line (:before (&rest args) +jg-log)
  (message "Running Next Line")
  )

(defun jg-interfere ()
  (when (and (eq this-command 'self-insert-command)
             (eq last-command-event ?w))
    (message "interfering")
    (setq this-command 'next-line)
    )
  )

(remove-hook! 'pre-command-hook 'jg-interfere)

(setq jg-log-wrap-count 0
      jg-log-wrap-go nil)

(defun jg-log-wrap (fn &rest args)
  (cl-incf jg-log-wrap-count)
  (let ((indent (apply 'concat (make-list jg-log-wrap-count "-")))
        result)
    (when jg-log-wrap-go (message ">>%s> : %s |- %s %s %s" indent fn real-this-command this-command unread-input-method-events))
    (setq result (apply fn args))
    (when jg-log-wrap-go (message "<%s<< : %s |- %s %s %s" indent fn real-this-command this-command unread-input-method-events))
    (when jg-log-wrap-count (cl-decf jg-log-wrap-count))
    result))

(defun jg-log-wrap-go-fn (fn &rest args)
  (setq jg-log-wrap-go t)
  (apply 'jg-log-wrap fn args)
  (setq jg-log-wrap-go nil)
  )

(setq jg-log-fns '(evil-escape-pre-command-hook
                     evil-escape--insert-func
                     evil-escape--delete-func
                     evil-ex
                     evil-ex-setup
                     evil-ex-teardown
                     evil-ex-update
                     self-insert-command
                     next-line
                     )
      jg-log-wrap-go-target 'evil-ex
      )

(cl-loop for fn in jg-log-fns
         do
         (if (eq jg-log-wrap-go-target fn)
             (advice-add fn :around #'jg-log-wrap-go-fn)
           (advice-add fn :around #'jg-log-wrap)
           )
         )

(cl-loop for fn in jg-log-fns
         do
         (advice-remove fn #'jg-log-wrap)
         (advice-remove fn #'jg-log-wrap-go-fn)
         )
