;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(after! evil
  (load! "+bindings")
  )
(after! (dired pyvenv-mode)
    """ Remove the annoying python-shell-setup advice """
    (add-transient-hook! 'dired-mode
      (map! :map dired-mode-map
        :localleader
        :n "v" 'pyvenv-activate
        )
      )
    )

(after! python-mode
  (add-hook! 'python-mode-hook #'outline-minor-mode)
  (add-hook! 'python-mode-hook #'+jg-personal-python-outline-regexp-override-hook)
  (add-hook! 'python-mode-hook #'+python-use-correct-flycheck-executables-h)
  (setq-hook! 'python-mode-hook tab-width python-indent-offset)

  (set-ligatures! 'python-mode
    ;; Functional
    :def    "def"
    :lambda "lambda"
    ;; Types
    :null   "None"
    :true   "True"
    :false  "False"
    :int    "int"
    :str    "str"
    :float  "float"
    :bool   "bool"
    :tuple  "tuple"
    ;; Flow
    :not    "not"
    :in     "in"
    :not-in "not in"
    :and    "and"
    :or     "or"
    :for    "for"
    :return "return"
    :yield  "yield")

  )
;; (after! (origami python-origami)
 ;;  (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
 ;;  (add-to-list 'origami-parser-alist '(python-mode . +jg-origami-python-parser))
 ;;  )
