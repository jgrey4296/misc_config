;;; +dap.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defvar +debugger--dap-alist
  `(((:lang cc +lsp)         :after ccls        :require (dap-lldb dap-gdb-lldb))
    ((:lang elixir +lsp)     :after elixir-mode :require dap-elixir)
    ((:lang go +lsp)         :after go-mode     :require dap-dlv-go)
    ((:lang java +lsp)       :after java-mode   :require lsp-java)
    ((:lang php +lsp)        :after php-mode    :require dap-php)
    ((:lang python +lsp)     :after python      :require dap-python)
    ((:lang ruby +lsp)       :after ruby-mode   :require dap-ruby)
    ((:lang rust +lsp)       :after rustic-mode :require (dap-lldb dap-cpptools))
    ((:lang javascript +lsp)
     :after (js2-mode typescript-mode)
     :require (dap-node dap-chrome dap-firefox ,@(if IS-WINDOWS '(dap-edge)))))
  "TODO")

(use-package! dap-mode
  :hook (dap-mode . dap-tooltip-mode)
  :init
  (setq dap-breakpoints-file (concat doom-data-dir "dap-breakpoints")
        dap-utils-extension-path (concat doom-data-dir "dap-extension/"))

  :config
  (pcase-dolist (`((,category . ,modules) :after ,after :require ,libs)
                 +debugger--dap-alist)
    (when (doom-module-active-p category (car modules) (cadr modules))
      (dolist (lib (ensure-list after))
        (with-eval-after-load lib
          (mapc #'require (ensure-list libs))))))

  ;; Activate this minor mode when dap is initialized
  (add-hook 'dap-session-created-hook #'+dap-running-session-mode)
  ;; Activate this minor mode when hitting a breakpoint in another file
  (add-hook 'dap-stopped-hook #'+dap-running-session-mode)
  ;; Activate this minor mode when stepping into code in another file
  (add-hook 'dap-stack-frame-changed-hook #'+debugger-dap-start-on-stack)

  )

(use-package! dap-ui
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode)
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 27, 2025
;; Modified:   July 27, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; +dap.el ends here
