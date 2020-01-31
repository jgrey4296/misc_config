(defun jg_layer/example_transient_func_setup ()
  """ Create an example transient state """

  (defun jg_layer/example_transient_func ()
    """ The simple function to call from within the transient state """
    (interactive)
    (print "blah")
    )

  (spacemacs|define-transient-state example_transient
    :title "an example transient state"
    :doc (concat "
[_q_]^^ Quit         [_h_]^^ Example Func
[_t_]^^ Example Func
")
    :bindings
    ("q" nil :exit t)
    ("t" jg_layer/example_transient_func)
    ("h" jg_layer/example_transient_func :exit t)
    )

  ;; Then register its activation
  (spacemacs/set-leader-keys "." 'spacemacs/example_transient-transient-state/body)
  )
(defun jg_layer/example-helm-setup ()
  """ An Example Helm Setup """

  (setq-default jg_layer/example_helm-source
                (helm-make-source "Find Image" 'helm-source-ffiles
                  :action '(("action" . (lambda (candidate) (insert candidate))))))

  (defun jg_layer/example_helm-call ()
    (interactive)
    (helm :sources jg_layer/example_helm-source
          :input "./"))
  )
