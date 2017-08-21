; based On https://www.emacswiki.org/emacs/ModeTutorial
;;For allowing code to run when the mode is run:
(defvar trie-mode-hook nil)

;;--------------------
;;Utilities
;;--------------------
(defvar trie-defined-depth-count 10)
(defvar trie-outermost-depth-face-count 3)
(defvar trie-depth-face-name "trie-depth-")
(defvar trie-depth-color-list '("color-26" "color-47" "color-99" "color-124"
                                "color-129" "color 142" "color-164"))

(defun flatten (lst)
  (letrec ((internal (lambda (x)
                   (cond
                     ((null x) nil)
                     ((atom x) (list x))
                     (t
                      (append (funcall internal (car x)) (funcall internal (cdr x))))))))
    (progn
      (assert (listp lst))
      (funcall internal lst))))

(defun make-list-as-big-as-n (lst n)
  (progn
    (assert (listp lst))
    (assert (numberp n))
    (letrec ((lenlst (length lst)))
      (if (>= lenlst n)
          lst
          (letrec ((repN (+ 1 (/ n lenlst)))
                   (newLst (flatten (-repeat repN lst))))
            (assert (>= (length newLst) n))
            newLst)))))

;;adapted from rainbow-blocks.el
;;returns a string of a face name
(defun trie-depth-face (depth)
  (let ((name (concat trie-depth-face-name
                      (number-to-string
                       (or
                        (and (<= depth trie-defined-depth-count)
                             depth)
                        ;;otherwise cycle
                        (+ 1 trie-outermost-depth-face-count
                           (mod (- depth trie-defined-depth-count 1)
                                (- trie-defined-depth-count
                                   trie-outermost-depth-face-count))))))))
    name))


(defmacro* trie-generate-face (name color)
  `(defface ,name
       (list (list t :foreground ,color :background "black"))
     "A Generated Face"
     :group 'trie-mode))

;;Create depth faces:
(defun trie-face-creation ()
  (letrec (
           ;;create the range
           (range (number-sequence 0 (- trie-defined-depth-count 1)))
           ;;create the names
           (names (map 'sequence 'trie-depth-face range))
           ;;Setup the color list at correct list length
           (colorList (make-list-as-big-as-n trie-depth-color-list
                                             trie-defined-depth-count))
           ;;pair with a foreground color
           (paired (-zip-pair names colorList)))
    ;;now create the faces
    (dolist (pair paired)
      (let ((newFaceName (car pair))
            (newFaceColor (cdr pair)))
        (progn
          (print (concat "Creating Face: " newFaceName))
          (print (format "%S" (macroexpand `(trie-generate-face ,(intern newFaceName)
                                                                newFaceColor))))
          (eval (macroexpand `(trie-generate-face ,(intern newFaceName) newFaceColor))))))))

(trie-face-creation)

;;--------------------
;;definitions
;;--------------------
(setq trie-keywords '("assert" "retract"))

;;generate regexp for keywords
(setq trie-keywords-regexp (regexp-opt trie-keywords 'words))


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
    map)
  "Keymap for Trie mode major mode")

;;--------------------
;;Keyword based syntax highlighting
;;Specify as a list of (matcher . facename )
;;Potential keywords: operators +-*/!.()""$~ < > != == -> @
;;--------------------
(defmacro stringToSymbol (name)
  `(intern-soft ,name))

(defun findFace (name)
  (seq-find (lambda (x) (string= (face-name x) name)) (face-list)))

(defmacro quoteVal (name)
  `(quote ,name))

(defmacro getQuotedFace (name)
  `(let ((v (stringToSymbol ,name)))
     (eval (macroexpand `(quoteVal ,v)))))
  


(defconst trie-font-lock-keywords
  (list
   ;;facts
   '("\\([.!]\\w+\\)+" (0 (trie-depth-face 9) t))
   ;;Rule name
   '("\\([.!]\\w+\\)+:$" (0 (trie-depth-face 0) t))
   ;;Rule end
   `("end$" (0 (trie-depth-face 0)))
   ;;Query
   '("\\([.!][$a-zA-Z_]+\\)+\\?" (0 font-lock-variable-name-face t))
   ;;Transform
   '("\\((\\$[a-zA-Z0-9_]+ [+*/-] [a-zA-Z0-9_.]+\\( ?-> ?\\$[a-zA-Z0-9_]+\\)?)\\)" . (0 font-lock-constant-face t))
   ;;Actions
   '("\\([a-zA-Z+@-]+(\\([.!\" 0-9]\\|\\w+\\)+)\\)" (0 (trie-depth-face 6) t))
   ;;Variables
   '("\\$[a-zA-Z0-9_]" (0 (trie-depth-face 5) t))
   )
  "Minimal highlighting expressions for trie mode")



;;--------------------
;;Indentation
;; Potential indent points:
;; newline ending with an EXOP, comma,
;; reset indent if prior line is empty
;; 
;;--------------------
;; TODO: indent after : only
(defun trie-indent-line()
  "Indent current-line as trie code"
  (interactive)
  (beginning-of-line)
  (if (bobp) ;;if at beginning of buffer
       ;;then:
      (indent-line-to 0)
      ;;else:
      (let ((not-indented t) cur-indent)
        (if (looking-at "^[ \t]*END_") ;;if line contains an END_
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
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for the trie-mode")

;;--------------------
;;Autoloading
;;--------------------
(add-to-list 'auto-mode-alist '("\\.trie\\'" . trie-mode))

;; --------------------
;;Entry Function
;;--------------------
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


;;todo later: set no longer needed variables to nil


(provide 'trie-mode)
