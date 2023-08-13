;; lookup.el -*- lexical-binding: t; -*-
(require 'ivy)
(require 'lsp-ivy)

;;;###autoload
(defun +jg-lsp-describe-workspace-symbol (arg)
  " lookup documentation for a workspace symbol "
  (interactive "P")
  (+jg-lsp--ivy-workspace-symbol (or (lsp-workspaces)
                                 (gethash (lsp-workspace-root default-directory)
                                          (lsp-session-folder->servers (lsp-session))))
                             "Workspace symbol: "
                             (when arg (thing-at-point 'symbol)))
  )

(defun +jg-lsp--ivy-workspace-symbol (workspaces prompt initial-input)
  "Search against WORKSPACES with PROMPT and INITIAL-INPUT."
  (unless workspaces
    (user-error "No LSP workspace active"))

  (message "Starting Ivy")
  (with-lsp-workspaces workspaces
    (let* ((non-essential t)
           (prev-query nil)
           (unfiltered-candidates '())
           (workspace-root (lsp-workspace-root)))
      (ivy-read prompt
                (lambda (user-input)
                  (let* ((parts (split-string user-input))
                         (query (or (car parts) ""))
                         (filter-regexps? (mapcar #'regexp-quote (cdr parts)))
                         )
                    (unless (string-equal prev-query query)
                      (setq unfiltered-candidates
                            (with-lsp-workspaces workspaces
                              (lsp-request-while-no-input "workspace/symbol"
                                                          (lsp-make-workspace-symbol-params :query query)))))
                    (setq prev-query query)
                    (--keep (lsp-ivy--transform-candidate it filter-regexps? workspace-root)
                            unfiltered-candidates)))
                :dynamic-collection t
                :require-match t
                :initial-input initial-input
                :action #'+jg-lsp--symbol-help
                )
      )
    )
  )

(defun +jg-lsp--symbol-help (sym-string)
  " get information for a symbol and open a help buffer for it"
  (+jg-lsp--help (get-text-property 0 'lsp-ivy-symbol sym-string))
  )

(lsp-defun +jg-lsp--help ((&SymbolInformation
                           :location (&Location :uri
                                                :range (&Range :start (&Position :line :character)))))
  (let ((contents (-some->>
                      (list :textDocument (list :uri uri) :position (list :line line :character character))
                    (lsp--make-request "textDocument/hover")
                    (lsp--send-request)
                    (lsp:hover-contents)))
        (lsp-help-buf-name "*lsp-help*"))
    (with-current-buffer (get-buffer-create lsp-help-buf-name)
      (let ((delay-mode-hooks t))
        (lsp-help-mode)
        (with-help-window lsp-help-buf-name
          (message "Contents were: %s" contents)
          (insert (string-trim-right (lsp--render-on-hover-content contents t)))))
      (run-mode-hooks)))
  )
