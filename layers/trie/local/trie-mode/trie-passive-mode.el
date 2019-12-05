;;based On https://www.emacswiki.org/emacs/ModeTutorial
;;For allowing code to run when the mode is run:
(require 'dash)
(require 'trie-face-macro)

(defgroup trie-passive-mode '() "Trie Mode Customizations")
;;--------------------
;; Mode Variables
;;--------------------
(defcustom trie-passive-mode-hook nil "Basic Hook For Trie Mode")
;;--------------------
;;Utilities
;;--------------------

;;--------------------
;;definitions
;;--------------------
(defconst trie-passive-keywords '("assert" "retract" "end"))
;;generate regexp for keywords
(defconst trie-passive-keywords-regexp (regexp-opt trie-passive-keywords))

;;--------------------
;;face definitions
;; use them as symbols 'blah in font-lock-keywords
;;--------------------
(defface trie-passive-rulename
  '((t
     :foreground "green"
     :background "black"
     :underline t))
  "Face for Rule names"
  :group 'trie-passive-mode)
(defface trie-passive-ruleend
  '((t
     :foreground "red"
     :background "black"
     :underline t
     ))
  "Face for the end of rules"
  :group 'trie-passive-mode)
(defface trie-passive-closure
  '((t
     :background "red"
     ))
  "Face for Enclosed sections"
  :group 'trie-passive-mode)


;;--------------------
;;Key bindings.
;;use sparse-keymap if only a few bindings
;;--------------------
(defvar trie-passive-mode-map
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
(defconst trie-passive-font-lock-keywords
  (list
   ;;Rule name
   `("^\\([[:word:].!]+\\):$" (0 "trie-rulename"))
   ;;punctuation
   `("\\." (0 ,(trie-depth-face 1)))
   `("!" (0 ,(trie-depth-face 2)))
   `("::" (0 ,(trie-depth-face 3) t))
   `("->\\|\\?" (0 ,(trie-depth-face 4) t))
   ;;Variables and tags
   `("#[[:word:]]+" (0 ,(trie-depth-face 5)))
   `("\\$[[:word:]]+" (0 ,(trie-depth-face 6)))
   ;;functions
   `("\\([-<>=%^*@+&~][[:word:]]*\\)" (1 ,(trie-depth-face 7)))
   ;;Words
   `("[[:word:]]" (0 ,(trie-depth-face 8)))
   ;;Closures
   `("[][()]" (0 ,(trie-depth-face 9)))
   `("[([]\.+[])]" (0 "trie-closure" t))

   )
  "Minimal highlighting expressions for trie mode")

;;--------------------
;;Indentation
;; Potential indent points:
;; newline ending with an EXOP, comma,
;; reset indent if prior line is empty
;;
;;--------------------
(defun trie-passive-indent-line()
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
              (if (looking-at "^.+:$")
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
(defvar trie-passive-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?$ "_" st)
    ;;underscores are valid parts of words:
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ "<12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?: ".:2" st)
    st)
  "Syntax table for the trie-passive-mode")

;;--------------------
;;Autoloading
;;--------------------
;; (add-to-list 'auto-mode-alist '("\\.trie\\'" . trie-passive-mode))


(defun trie-passive-syntactic-face-function (parse-state)
  'font-lock-comment-face
  )

;; --------------------
;;Entry Function
;;--------------------
(define-derived-mode trie-passive-mode fundamental-mode "Trie Passive Mode"
  "Major Mode for creating rules using tries"
  (interactive)
  (kill-all-local-variables)
  (use-local-map trie-passive-mode-map)
  (let ((base-locks (reverse trie-passive-font-lock-keywords))
        (keywords (list trie-passive-keywords-regexp 0 "trie-ruleend" t)))
    (push keywords base-locks)
    (set (make-local-variable 'font-lock-defaults) (list (reverse base-locks) nil))
    )
  (set (make-local-variable 'font-lock-syntactic-face-function) 'trie-passive-syntactic-face-function)
  (set (make-local-variable 'indent-line-function) 'trie-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table trie-passive-mode-syntax-table)
  (setq major-mode 'trie-passive-mode)
  (setq mode-name "TRIE Passive")
  (run-mode-hooks)
  )

;;todo later: set no longer needed variables to nil

(provide 'trie-passive-mode)
