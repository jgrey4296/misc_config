;;; +lsp.el -*- lexical-binding: t; -*-

(doom-log "Loading Python LSP")

(after! lsp-mode
  (local-load! "lsp/custom-pylsp")
  (local-load! "lsp/custom-pyright-lsp")
  (local-load! "lsp/custom-ruff-lsp")
  (local-load! "lsp/custom-lsp-jedi")

  (lsp-register-client ;; pyright
   (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda ()
                                            (cons (lsp-package-path 'pyright)
                                                  lsp-pyright-langserver-command-args)))
    :major-modes '(python-mode python-ts-mode)
    :server-id 'jg-pyright
    :multi-root lsp-pyright-multi-root
    :priority 2
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        ;; we send empty settings initially, LSP server will ask for the
                        ;; configuration of each workspace folder later separately
                        (lsp--set-configuration (make-hash-table :test 'equal))))
    :download-server-fn (lambda (_client callback error-callback _update?)
                          (lsp-package-ensure 'pyright callback error-callback))
    :notification-handlers (lsp-ht ("pyright/beginProgress"  'lsp-pyright--begin-progress-callback)
                                   ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
                                   ("pyright/endProgress"    'lsp-pyright--end-progress-callback)
                                   )
    )
   )

  (lsp-register-client ;; pylsp
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-pylsp-server-command))
                    :activation-fn (lsp-activate-on "python")
                    :priority -1
                    :server-id 'jg-pylsp
                    :library-folders-fn (lambda (_workspace) lsp-clients-pylsp-library-directories)
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration (lsp-configuration-section "pylsp"))))
                    )
   )

  (lsp-register-client ;; ruff
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () lsp-ruff-lsp-server-command))
    :activation-fn (lsp-activate-on "python")
    :server-id 'jg-ruff
    :priority 3
    :add-on? t
    :initialization-options
    (lambda ()
      (list :settings
            (list :args                           lsp-ruff-lsp-ruff-args
                  :logLevel                       lsp-ruff-lsp-log-level
                  :path                           lsp-ruff-lsp-ruff-path
                  :interpreter                    (vector lsp-ruff-lsp-python-path)
                  :showNotifications              lsp-ruff-lsp-show-notifications
                  :organizeImports                (lsp-json-bool lsp-ruff-lsp-advertize-organize-imports)
                  :fixAll                         (lsp-json-bool lsp-ruff-lsp-advertize-fix-all)
                  :importStrategy                 lsp-ruff-lsp-import-strategy
                  )
            )
      )
    )
   )



  )
