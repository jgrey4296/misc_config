;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(doom-log "Config JG Python")

(load! "+manifest-mode")
(load! "+vars")
(load! "+funcs")
(load! "+hooks")
(after! jg-bindings-total
  (load! "+bindings")
  (load! "+nav")
  )
(load! "+advice")
(load! "+env")
(after! python
  (load! "+derived-modes")
  )

(use-package! pyimport
  :demand
  )
(use-package! lsp-jedi
  :defer)

(use-package! python-mode
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil)

  (when (modulep! +lsp)
    (add-hook 'python-mode-local-vars-hook #'lsp! 'append)
    ;; Use "mspyls" in eglot if in PATH
    (when (executable-find "Microsoft.Python.LanguageServer")
      (set-eglot-client! 'python-mode '("Microsoft.Python.LanguageServer"))))

  (when (modulep! +tree-sitter)
    (add-hook 'python-mode-local-vars-hook #'tree-sitter! 'append))
  :config
  (set-repl-handler! 'python-mode #'+python/open-repl
    :persist t
    :send-region #'python-shell-send-region
    :send-buffer #'python-shell-send-buffer)
  (set-docsets! '(python-mode inferior-python-mode) "Python 3" "NumPy" "SciPy" "Pandas")

  (set-ligatures! 'python-mode
    ;; Functional
    :def "def"
    :lambda "lambda"
    ;; Types
    :null "None"
    :true "True" :false "False"
    :int "int" :str "str"
    :float "float"
    :bool "bool"
    :tuple "tuple"
    ;; Flow
    :not "not"
    :in "in" :not-in "not in"
    :and "and" :or "or"
    :for "for"
    :return "return" :yield "yield")

  ;; Stop the spam!
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems link the unversioned one to Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  (add-hook! 'python-mode-hook
    (defun +python-use-correct-flycheck-executables-h ()
      "Use the correct Python executables for Flycheck."
      (let ((executable python-shell-interpreter))
        (save-excursion
          (goto-char (point-min))
          (save-match-data
            (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                      (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
              (setq executable (substring-no-properties (match-string 1))))))
        ;; Try to compile using the appropriate version of Python for
        ;; the file.
        (setq-local flycheck-python-pycompile-executable executable)
        ;; We might be running inside a virtualenv, in which case the
        ;; modules won't be available. But calling the executables
        ;; directly will work.
        (setq-local flycheck-python-pylint-executable "pylint")
        (setq-local flycheck-python-flake8-executable "flake8"))))

  ;; Affects pyenv and conda
  (when (modulep! :ui modeline)
    (advice-add #'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
    (advice-add #'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h))

  (setq-hook! 'python-mode-hook tab-width python-indent-offset))


(use-package-hook! python-mode :post-config
  (setq python-mode-hook nil)
  (setq python-mode-local-vars-hook nil)
  (add-hook! 'python-mode-hook #'outline-minor-mode
             #'+jg-python-outline-regexp-override-hook
             #'+python-use-correct-flycheck-executables-h
             #'doom-modeline-env-setup-python
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             #'doom--setq-tab-width-for-python-mode-h
             )
  ;; Always add auto-hide as the last thing
  (add-hook! 'python-mode-hook :depth 100
             '+jg-python-auto-hide
             )
  (setq-hook! 'python-mode-hook tab-width python-indent-offset)
)

(use-package-hook! anaconda-mode :post-config
  (+jg-python-conda-binding-override)
  )

(use-package! lsp-pyright
  :after lsp-mode
  :init
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-disabled-clients 'pylsp)
  (add-to-list 'lsp-disabled-clients 'mspyls)
)



  ;; (use-package! lsp-python-ms
  ;;   :unless (modulep! :lang python +pyright)
  ;;   :after lsp-mode
  ;;   :preface
  ;;   (after! python
  ;;     (setq lsp-python-ms-python-executable-cmd python-shell-interpreter)))


;; (use-package! lsp-jedi
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (add-to-list 'lsp-enabled-clients 'jedi)
;;   )

;; (after! (origami python-origami)
 ;;  (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
 ;;  (add-to-list 'origami-parser-alist '(python-mode . +jg-origami-python-parser))
 ;;  )
