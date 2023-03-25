;;; local-state-bind.el -*- lexical-binding: t; -*-


(general-define-key :states 'visual
                    :major-modes t
                    :prefix doom-localleader-key
                    :keymaps '(emacs-lisp-mode-map)
                    "X" (cmd! (message "visual"))
      )

(map! :map emacs-lisp-mode-map
          :localleader
          :desc "vis" "X" (cmd! (message "visual"))
)

(+jg-map! :map emacs-lisp-mode-map
          :localleader
          ("X"
           :desc "visual" :v (cmd! (message "visual"))
           :desc "normal" :n (cmd! (message "norm"))
           )
          )

(defmacro +jg-map! (&rest args)
  (let ((states (make-hash-table))
        keymaps built-defines
        cbind cdesc cstates ccmd)
    (while args
      (let ((current (pop args)))
        (cond ((listp current)
               ;; Handle multi binds to diff states
               (cl-loop for binding in (+jg-loop-list current)
                        do
                        (push (cdr binding) (gethash (car binding) states))
                        )
               )
              ((keywordp current)
               (pcase current
                 (:after

                  )
                 (:map

                  )
                 (:desc

                  )
                 )
               ;; handle desc, state selection
               )
              ((stringp current)
               ;; handle keybind
               )
              (t
               ;; map states
               (setq cstates (doom--map-keyword-to-states current))
               )
              )
        )
      (if (and cstate cbind cdesc)
          (progn
            ;; put into state hash
            ;;clear
            ))
      )
    ;; after while:
    (cl-loop for state in (hash-table-keys states)
             do
             ;; get the states bindings,
             (push (+jg-build-general-define state bindings)
                   build-defines)
             )

    (macroexp-progn built-defines)
    )
  )

(defun +jg-loop-list (lst)
  (cl-assert (stringp (car lst)))
  (let ((bind (pop lst))
         state-map cdesc cstate ccmd)
    (while lst
      (let ((curr (pop lst)))
        ;; get descs, states, cmds

        )
      )
    (cl-loop for state in (hash-table-keys state-map)
             collect

             )
    )
  )


(defun +jg-build-general-define (state bindings)

  (append '(general-define-key :states ,state
                               :prefix doom-localleader-key
                               :major-modes t
                               :keymaps '(emacs-lisp-mode-map))
          bindings
          )
  )
