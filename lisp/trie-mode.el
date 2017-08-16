;; Based on https://www.emacswiki.org/emacs/ModeTutorial
;;For allowing code to run when the mode is run:
(defvar trie-mode-hook nil)

;;Key bindings. use sparse-keymap if only a few bindings
(defvar trie-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)

    map)
  "Keymap for Trie mode major mode")
    
;;Autoloading
(add-to-list 'auto-mode-alist '("\\.trie\\'" . trie-mode))

;;Keyword based syntax highlighting
;; Specify as a list of (matcher . facename )
(defconst trie-font-lock-keywords-1
  (list
   '("\\<ENTER" . font-lock-builtin-face)
   '("\\<assert" . font-lock-builtin-face)
   '("\\<retract" . font-lock-variable-name-face))
  "Minimal highlighting expressions for trie mode")

(defconst trie-font-lock-keywords-2
  (append trie-font-lock-keywords-1
          (list
           '("\\<Blah" . font-lock-keyword-face)
           '("\\<True" . font-lock-constant-face)))
  "Additional keywords to highlight ")

(defvar trie-font-lock-keywords trie-font-lock-keywords-2)

;;Indentation
(defun trie-indent-line()
  "Indent current-line as trie code"
  (interactive)
  (beginning-of-line)
  (if (bobp) ;;if at beginning of buffer
      (indent-line-to 0) ;;then
      ;;else:
      (let ((not-indented t) cur-indent)
        (if (looking-at "^[ \t]*END_") ;;if line contains and END_
            (progn
              (save-excursion ;;save where we are
                (forward-line -1) ;;go back a line
                ;;then deindent:
                (setq cur-indent (- (current-indentation) (default-tab-width)))
                ;;guard:
                (if (< cur-indent 0)
                    (setq cur-indent 0))))
            ;;else:
            (save-excursion
              (while not-indented
                (forward-line -1)
                (if (looking-at "^[ \t]*END_")
                    (progn
                      (setq cur-indent (current-indentation))
                      (setq not-indented nil))
                    ;;otherwise
                    (if (looking-at "^[ \t]*ENTER")
                        (progn
                          (setq cur-indent (+ (current-indentation) default-tab-width))
                          (setq not-indented nil))
                        (if (bobp)
                            (setq not-indented nil)))))))
        (if cur-indent
            (indent-line-to cur-indent)
            (indent-line-to 0)))))

;;Define the syntax table, for word definitions etc
;;modify-syntax-entry: character, class/flag, syntable)
;;classes/syntax flags: include 'w': word, '.':punctuation,
;; see: C-h ? r elisp manual syntax-tables
(defvar trie-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st) ;;underscores are valid parts of words
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for the trie-mode")

;;Entry Function
(defun trie-mode ()
  "Major mode for editing tries"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table trie-mode-syntax-table)
  (use-local-map trie-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(trie-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'trie-indent-line)
  (setq major-mode 'trie-mode)
  (setq mode-name "TRIE")
  (run-hooks 'trie-mode-hook))


(provide 'trie-mode)

