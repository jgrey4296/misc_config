; based On https://www.emacswiki.org/emacs/ModeTutorial
;;For allowing code to run when the mode is run:
(require 'dash)

;;--------------------
;; Mode Variables
;;--------------------
(defcustom trie-mode-hook nil "Basic Hook For Trie Mode")
;;--------------------
;;Utilities
;;--------------------

;;--------------------
;;definitions
;;--------------------
(defconst trie-keywords '("assert" "retract"))

;;generate regexp for keywords
(defconst trie-keywords-regexp (regexp-opt trie-keywords 'words))


;;--------------------
;;face definitions
;; use them as symbols 'blah in font-lock-keywords
;;--------------------
(defface trie-rulename
    '((t
       :foreground "red"
       :background "black"
       :underline t))
  "Face for Rule names"
  :group 'trie-mode)
(defface trie-ruleend
    '((t
       :foreground "red"
       :background "black"
       ))
  "Face for the end of rules"
  :group 'trie-mode)

;;--------------------
;;Key bindings.
;;use sparse-keymap if only a few bindings
;;--------------------
(defvar trie-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    ;;any potential functions? add particular structures?
    ;;TODO: M-RET to prev-line
    map)
  "Keymap for Trie mode major mode")

;;--------------------
;;Keyword based syntax highlighting
;;Specify as a list of (matcher . facename )
;;Potential keywords: operators +-*/!.()""$~ < > != == -> @
;;--------------------
(defconst trie-font-lock-keywords
  (list
   ;;facts
   '("\\(\\.[$.!a-zA-Z0-9_\]+\\((.*)\\)?\\)+\\(\\?$\\)?"
     (0 (trie-depth-face 9) t)
     (2 (trie-depth-face 6) t t)
     (3 (trie-depth-face 1) t t))
   ;;Rule name
   '("\\(\\..+\\)+:$" (0 (trie-depth-face 0) t))
   ;;Rule end
   `("end$" (0 (trie-depth-face 0)))
   ;;Transform
   '("\\(\\$[a-zA-Z0-9_]+ [+*/-] [$a-zA-Z0-9_.]+\\( ?-> ?\\$[a-zA-Z0-9_]+\\)?\\)"
     (0 font-lock-constant-face t))
   ;;Actions
   '("\\([a-zA-Z+@-]+( ?\\)\\(\\(.\\)+\\)\\( ?)\\)"
     (1 (trie-depth-face 6) t)
     (2 (trie-depth-face 9) )
     (4 (trie-depth-face 6) t))
   ;;Variables
   '("\\$[a-zA-Z0-9_]+" (0 (trie-depth-face 5) t))
   ;;Exclusion op
   '("!" (0 "trie-rulename" t))
   )
  "Minimal highlighting expressions for trie mode")

;;--------------------
;;Indentation
;; Potential indent points:
;; newline ending with an EXOP, comma,
;; reset indent if prior line is empty
;;
;;--------------------
(defun trie-indent-line()
  "Indent current-line as trie code"
  (interactive)
  (beginning-of-line)
  (if (bobp) ;;if at beginning of buffer
       ;;then:
      (indent-line-to 0)
      ;;else:
      (let ((not-indented t) cur-indent)
        (if (looking-at "^[ \t]*end$") ;;if line contains an END_
            (progn
              (save-excursion ;;save where we are
                (forward-line -1) ;;go back a line
                ;;then deindent:
                (setq cur-indent (- (current-indentation) tab-width))
                ;;guard:
                (if (< cur-indent 0)
                    (setq cur-indent 0))))
            ;;else:
            (save-excursion
              (while not-indented
                (forward-line -1)
                (if (looking-at "^[ \t]*end$")
                    (progn
                      (setq cur-indent (current-indentation))
                      (setq not-indented nil))
                    ;;otherwise
                    (if (looking-at "^.+:")
                        (progn
                          (setq cur-indent (+ (current-indentation) tab-width))
                          (setq not-indented nil))
                        (if (bobp)
                            (setq not-indented nil)))))))
        (if cur-indent
            (indent-line-to cur-indent)
            (indent-line-to 0)))))

;;--------------------
;;Syntax Table
;;--------------------
;;Define the syntax table, for word definitions etc
;;modify-syntax-entry: character, class/flag, syntable)
;;classes/syntax flags: include 'w': word, '.':punctuation,
;; see: C-h ? r elisp manual syntax-tables
(defvar trie-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?$ "_" st)
    (modify-syntax-entry ?_ "w" st) ;;underscores are valid parts of words
    (modify-syntax-entry ?/ "< 12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"\"" st)

    st)
  "Syntax table for the trie-mode")

;;--------------------
;;Autoloading
;;--------------------
(add-to-list 'auto-mode-alist '("\\.trie\\'" . trie-mode))

;; --------------------
;;Entry Function
;;--------------------
(define-derived-mode trie-mode fundamental-mode "Trie Mode"
  "Major Mode for creating rules using tries"
  (interactive)
  (kill-all-local-variables)
  (use-local-map trie-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(trie-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'trie-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table trie-mode-syntax-table)
  (setq major-mode 'trie-mode)
  (setq mode-name "TRIE")
  (run-mode-hooks)
)

;;todo later: set no longer needed variables to nil

(provide 'trie-mode)
