;;; soar-mode.el -*- lexical-binding: t; -*-
;;-- imports
(require 'soar-faces)
(require 'soar-comint)

;;-- end imports

;;-- vars

(defcustom soar-executable "soar"
  "The executable to use for soar"
  :type "string")

;;-- end vars

;;-- keymap

(defvar-local soar-mode-map
  (make-sparse-keymap))

;;-- end keymap

;;-- font-lock

(defconst soar-font-lock-keywords
  (rx-let ((w (x) (: x (0+ blank)))
           (g (x) (group x))
           (ln (: punctuation line-end))
           (word+ (group word-start (+ (| word punct)) (0+ blank)))
           (basic-syms (| "@" "+" "!" "<-" "?" "-" "&"))
           (basic-kws  (| "percept" "self" "include" "register_function"))
           (agent-ids (| "beliefs" "goals" "debug" "verbose" "ag-class" "ag-arch" "ag-bb-class" "myparameter" "instances" "join" "focus" "roles"))
           (org-ids   (| "responsible-for" "debug" "group" "players" "owner"))
           (comparisons (| "<>" ?< ?> "<=" ">=" "=" "<=>"))
           (operators (| "-->"))
           )
    (list
     `(,(rx line-start (g (w "sp")) (w "{") word+)
       (1 'font-lock-keyword-face)
       (2 'font-lock-variable-name-face))
     `(,(rx (g (: "<" word+ ">")))
       (1 'font-lock-variable-name-face t))
     `(,(rx (g (: "^" word+)))
       (1 font-lock-constant-face t))
     `(,(rx ?^ (regexp "[-[:alpha:]]+")) (0 'soar-face-1))
     `(,(rx "(" (group (or "state" "impasse")))
       (1 'soar-face-2))
     `(,(rx ":" (or "o-support"
                    "i-support"
                    "chunk"
                    "default"))
       (0 'soar-face-2))
     `(,(rx (or ?( ?) ))
       (0 'soar-face-0))
     `(,(rx (| comparisons operators))
       (0 'font-lock-operator-face)
       )
     )
    )
  "Highlighting for soar-mode"
  )

()
;;-- end font-lock

;;-- syntax
(defvar soar-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?$ "_" st)
    ;;underscores are valid parts of words:
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "<12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?: ".:2" st)
    st)
  "Syntax table for the soar-mode")

;;-- end syntax

(define-derived-mode soar-mode fundamental-mode
  "soar"
  "Major mode for use of soar"
  (interactive)
  (kill-all-local-variables)
  (use-local-map soar-mode-map)

  (set (make-local-variable 'font-lock-defaults) (list soar-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'soar-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'soar-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table soar-mode-syntax-table)
  ;;
  (setq major-mode 'soar-mode)
  (setq mode-name "soar")
  (outline-minor-mode)
  (yas-minor-mode)
  (run-mode-hooks)
  )
(add-to-list 'auto-mode-alist '("\\.soar" . soar-mode))

(provide 'soar-mode)
