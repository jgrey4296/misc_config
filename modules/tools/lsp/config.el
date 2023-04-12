;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 07, 2023
;; Modified: April 07, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(load! "+vars")
(load! "+minor-modes")
(after! evil
  (load! "+bindings")
  )

(use-package! lsp-mode
  :commands lsp-install-server
  :init
  ;; Don't touch ~/.emacs.d, which could be purged without warning
  (setq lsp-session-file (concat doom-cache-dir "lsp-session")
        lsp-server-install-dir (concat doom-data-dir "lsp"))
  ;; Don't auto-kill LSP server after last workspace buffer is killed, because I
  ;; will do it for you, after `+lsp-defer-shutdown' seconds.
  (setq lsp-keep-workspace-alive nil)

  ;; NOTE I tweak LSP's defaults in order to make its more expensive or imposing
  ;;      features opt-in. Some servers implement these poorly and, in most
  ;;      cases, it's safer to rely on Emacs' native mechanisms (eldoc vs
  ;;      lsp-ui-doc, open in popup vs sideline, etc).

  ;; Disable features that have great potential to be slow.
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  ;; Reduce unexpected modifications to code
  (setq lsp-enable-on-type-formatting nil)
  ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
  (setq lsp-headerline-breadcrumb-enable nil)

  ;; Let doom bind the lsp keymap.
  (when (modulep! :config default +bindings)
    (setq lsp-keymap-prefix nil))

  :config
  (add-to-list 'doom-debug-variables 'lsp-log-io)

  (setq lsp-intelephense-storage-path (concat doom-data-dir "lsp-intelephense/")
        lsp-vetur-global-snippets-dir
        (expand-file-name
         "vetur" (or (bound-and-true-p +snippets-dir)
                     (concat doom-user-dir "snippets/")))
        lsp-xml-jar-file (expand-file-name "org.eclipse.lsp4xml-0.3.0-uber.jar" lsp-server-install-dir)
        lsp-groovy-server-file (expand-file-name "groovy-language-server-all.jar" lsp-server-install-dir))

  ;; REVIEW Remove this once this is fixed upstream.
  (add-to-list 'lsp-client-packages 'lsp-racket)

  (add-hook! 'doom-escape-hook
    (defun +lsp-signature-stop-maybe-h ()
      "Close the displayed `lsp-signature'."
      (when lsp-signature-mode
        (lsp-signature-stop)
        t)))

  (set-popup-rule! "^\\*lsp-\\(help\\|install\\)" :size 0.35 :quit t :select t)
  (set-lookup-handlers! 'lsp-mode
    :definition #'+lsp-lookup-definition-handler
    :references #'+lsp-lookup-references-handler
    :documentation '(lsp-describe-thing-at-point :async t)
    :implementations '(lsp-find-implementation :async t)
    :type-definition #'lsp-find-type-definition)

  (defadvice! +lsp--respect-user-defined-checkers-a (fn &rest args)
    "Ensure user-defined `flycheck-checker' isn't overwritten by `lsp'."
    :around #'lsp-diagnostics-flycheck-enable
    (if flycheck-checker
        (let ((old-checker flycheck-checker))
          (apply fn args)
          (setq-local flycheck-checker old-checker))
      (apply fn args)))

  (defadvice! +jg-lsp-dont-select-session (fn &rest args)
    " Dont auto select lsp session buffer "
    :around #'lsp-describe-session
    (let ((curr (selected-window)))
      (apply fn args)
      (select-window curr)
      )
    )

  (add-hook! 'lsp-mode-hook #'+lsp-optimization-mode)

  (when (modulep! :completion company)
    (add-hook! 'lsp-completion-mode-hook
      (defun +lsp-init-company-backends-h ()
        (when lsp-completion-mode
          (set (make-local-variable 'company-backends)
               (cons +lsp-company-backends
                     (remove +lsp-company-backends
                             (remq 'company-capf company-backends))))))))

  (defvar +lsp--deferred-shutdown-timer nil)
  (defadvice! +lsp-defer-server-shutdown-a (fn &optional restart)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart
            (null +lsp-defer-shutdown)
            (= +lsp-defer-shutdown 0))
        (prog1 (funcall fn restart)
          (+lsp-optimization-mode -1))
      (when (timerp +lsp--deferred-shutdown-timer)
        (cancel-timer +lsp--deferred-shutdown-timer))
      (setq +lsp--deferred-shutdown-timer
            (run-at-time
             (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
             nil (lambda (workspace)
                   (with-lsp-workspace workspace
                     (unless (lsp--workspace-buffers workspace)
                       (let ((lsp-restart 'ignore))
                         (funcall fn))
                       (+lsp-optimization-mode -1))))
             lsp--cur-workspace))))

  (when (modulep! :ui modeline +light)
    (defvar-local lsp-modeline-icon nil)

    (add-hook! '(lsp-before-initialize-hook
                 lsp-after-initialize-hook
                 lsp-after-uninitialized-functions
                 lsp-before-open-hook
                 lsp-after-open-hook)
      (defun +lsp-update-modeline (&rest _)
        "Update modeline with lsp state."
        (let* ((workspaces (lsp-workspaces))
               (face (if workspaces 'success 'warning))
               (label (if workspaces "LSP Connected" "LSP Disconnected")))
          (setq lsp-modeline-icon (concat
                                   " "
                                   (+modeline-format-icon 'faicon "rocket" "" face label -0.0575)
                                   " "))
          (add-to-list 'global-mode-string
                       '(t (:eval lsp-modeline-icon))
                       'append))))))

(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (defadvice! +lsp--use-hook-instead-a (fn &rest args)
    "Change `lsp--auto-configure' to not force `lsp-ui-mode' on us. Using a hook
instead is more sensible."
    :around #'lsp--auto-configure
    (letf! ((#'lsp-ui-mode #'ignore))
      (apply fn args)))

  :config
  (when (modulep! +peek)
    (set-lookup-handlers! 'lsp-ui-mode
      :definition 'lsp-ui-peek-find-definitions
      :implementations 'lsp-ui-peek-find-implementation
      :references 'lsp-ui-peek-find-references
      :async t))

  (setq lsp-ui-peek-enable (modulep! +peek)
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72         ; 150 (default) is too wide
        lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
        lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil
        ;; Re-enable icon scaling (it's disabled by default upstream for Emacs
        ;; 26.x compatibility; see emacs-lsp/lsp-ui#573)
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)

  (map! :map lsp-ui-peek-mode-map
        "j"   #'lsp-ui-peek--select-next
        "k"   #'lsp-ui-peek--select-prev
        "C-k" #'lsp-ui-peek--select-prev-file
        "C-j" #'lsp-ui-peek--select-next-file))

(use-package! lsp-ivy
  :when (modulep! :completion ivy)
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)

(use-package! eglot
  :commands eglot eglot-ensure
  :hook (eglot-managed-mode . +lsp-optimization-mode)
  :init
  (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
        ;;      its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil)
  (when (modulep! :checkers syntax)
    (setq eglot-stay-out-of '(flymake)))

  :config
  (set-popup-rule! "^\\*eglot-help" :size 0.15 :quit t :select t)
  (set-lookup-handlers! 'eglot--managed-mode
    :definition      #'xref-find-definitions
    :references      #'xref-find-references
    :implementations #'eglot-find-implementation
    :type-definition #'eglot-find-typeDefinition
    :documentation   #'+eglot-lookup-documentation)

  (add-to-list 'doom-debug-variables '(eglot-events-buffer-size . 0))

  (defadvice! +lsp--defer-server-shutdown-a (fn &optional server)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    :around #'eglot--managed-mode
    (letf! (defun eglot-shutdown (server)
             (if (or (null +lsp-defer-shutdown)
                     (eq +lsp-defer-shutdown 0))
                 (prog1 (funcall eglot-shutdown server)
                   (+lsp-optimization-mode -1))
               (run-at-time
                (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
                nil (lambda (server)
                      (unless (eglot--managed-buffers server)
                        (prog1 (funcall eglot-shutdown server)
                          (+lsp-optimization-mode -1))))
                server)))
      (funcall fn server))))

(use-package! flycheck-eglot
  :when (modulep! :checkers syntax)
  :hook (eglot-managed-mode . flycheck-eglot-mode))

;;; config.el ends here
