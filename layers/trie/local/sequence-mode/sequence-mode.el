;; A Custom mode adapting org-table

(require 'dash)
(require 'org-table)
(require 'kahnsort)
;; add transient / hydra
;; add helm

;;--------------------
;; Mode Variables
;;--------------------
(defconst sequence/left-tab "SequenceMLeft")
(defconst sequence/right-tab "SequenceMRight")

(defvar sequence/overlays '())
(defvar sequence/free-overlays '())
(defvar sequence/overlay-max 20)
(defvar sequence/inspector-overlay nil)
;;--------------------
;; Overlays
;;--------------------
(defun sequence/make-overlay (beg end type)
  (let ((overlay (if sequence/free-overlays (pop sequence/free-overlays) (make-overlay 1 2)))
        (color "green"))
    (if (not (-contains? sequence/overlays overlay))
          (push overlay sequence/overlays))
    (setq color (cond ((equal type :point) "green")
                      ((equal type :input) "blue")
                      ((equal type :output) "orange")))
    (overlay-put overlay 'face `((foreground-color . ,color)))
  (move-overlay overlay beg end)

  ))

(defun sequence/clear-overlays ()
  (loop for x in sequence/overlays do
        (push x sequence/free-overlays)
        (delete-overlay x)
        )
  (setq-local sequence/overlays '())
  )

(defun sequence/set-overlays-for-current ()
  " Set overlays for the currently selected cell "
  (let* ((row (org-table-current-line))
         (col (org-table-current-column))
         (value (substring-no-properties (org-table-get row col)))
         (value-hash (sequence/get-table-prop :sequence/value-hashmap))
         (value-obj (gethash value value-hash))
         ;; Get the inputs and outputs
         (inputs (sequence/get value-obj :inputs))
         (outputs (sequence/get value-obj :outputs))
         (wind (get-buffer-window "*Seq Info*"))
         )

    (if wind
        (with-current-buffer "*Seq Info*"
          (goto-char (point-min))
          (search-forward-regexp (format "^%s" value))
          (if (not (overlayp sequence/inspector-overlay))
              (progn (setq sequence/inspector-overlay (make-overlay 1 2))
                     (overlay-put sequence/inspector-overlay 'face '((foreground-color . "green")))))
          (move-overlay sequence/inspector-overlay
                        (line-beginning-position)
                        (point)
                        (get-buffer "*Seq Info*"))))

    (save-excursion
      (goto-char (org-table-begin))
      (search-forward value)
      (sequence/make-overlay
       (progn (skip-chars-backward "^|") (point))
       (progn (skip-chars-forward "^|") (point))
       :point)

      (loop for x in inputs do
            (goto-char (org-table-begin))
            (if (sequence/is-terminal-p x)
                (progn (search-forward x)
                       (sequence/make-overlay
                        (progn (skip-chars-backward "^|") (point))
                        (progn (skip-chars-forward "^|") (point))
                        :input)))
            (search-forward x)
            (sequence/make-overlay
             (progn (skip-chars-backward "^|") (point))
             (progn (skip-chars-forward "^|") (point))
             :input))

      (loop for x in outputs do
            (goto-char (org-table-begin))
            (if (sequence/is-terminal-p x)
                (progn (search-forward x)
                       (sequence/make-overlay
                        (progn (skip-chars-backward "^|") (point))
                        (progn (skip-chars-forward "^|") (point))
                        :output)))
            (search-forward x)
            (sequence/make-overlay
             (progn (skip-chars-backward "^|") (point))
             (progn (skip-chars-forward "^|") (point))
             :output))


      )
    )
  )


;;--------------------
;;User Motion
;;--------------------
(defun sequence/user-inc-column (count)
  " Increment the user point by a column,
    Updating the highlights as well "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (progn
        (org-table-goto-column (+ (if count (prefix-numeric-value count) 1) (org-table-current-column)))
        (sequence/clear-overlays)
        (sequence/set-overlays-for-current)
        )
      ;;No
    (progn (sequence/clear-overlays)
      (evil-forward-char (prefix-numeric-value count))))
  )

(defun sequence/user-dec-column (count)
  "Decrement the user point by a column "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (progn
        (org-table-goto-column (- (org-table-current-column) (if count (prefix-numeric-value count) 1)))
        (sequence/clear-overlays)
        (sequence/set-overlays-for-current)
        )
      ;;No
    (progn (sequence/clear-overlays)
      (evil-backward-char (prefix-numeric-value count))))
  )

(defun sequence/user-dec-line (count)
  "decrement the user point by a line "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (progn (let ((row (org-table-current-line))
                   (col (org-table-current-column)))
               (org-table-goto-line (- row (if count (prefix-numeric-value count) 1)))
               (org-table-goto-column col))
             (sequence/clear-overlays)
             (sequence/set-overlays-for-current)
        )
      ;;No
    (progn (sequence/clear-overlays)
      (evil-previous-line (prefix-numeric-value count))))
  )

(defun sequence/user-inc-line (count)
  "increment the user point by a line "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (progn (let ((row (org-table-current-line))
                   (col (org-table-current-column)))
               (org-table-goto-line (+ row (if count (prefix-numeric-value count) 1)))
               (org-table-goto-column col))
             (sequence/clear-overlays)
             (sequence/set-overlays-for-current)
        )
      ;;No
    (progn (sequence/clear-overlays)
      (evil-next-line (prefix-numeric-value count))))
  )

;;--------------------
;; Utilities
;;--------------------

(defun sequence/horizontal-recenter ()
  " Force horizontal recenter, based on:
https://stackoverflow.com/questions/1249497 "
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))

(defun sequence/get-table-prop (sym)
  " Get a property from the current table "
  (get-text-property (org-table-begin) sym))

(defun sequence/push-table-prop (sym val)
  "Push a value to a list property of the current table"
  (let ((lst (get-text-property (org-table-begin) sym)))
    (push val lst)
    (put-text-property (org-table-begin) (org-table-end) sym lst)))

(defun sequence/is-terminal-p (str)
  (-contains? (sequence/get-table-prop :sequence/terminals) str))

(defun sequence/inputless ()
  (let* ((value-map (sequence/get-table-prop :sequence/value-hashmap)))
    (-non-nil (mapcar (lambda (x) (if (sequence/get x :inputs) (sequence/get x :name))) (hash-table-values value-map)))))

(defun sequence/analyze-table ()
  " Update analytics on the table "
  (message "Analyzing table")
  (org-table-analyze)
  ;; create column sets
  (let* ((depth-hash (sequence/get-table-prop :sequence/depth-hashmap))
        (dh_max (if (hash-table-empty-p depth-hash) 0 (-max (hash-table-values depth-hash))))
        (true-max (max 3 (+ 1 dh_max))))
    (put-text-property (org-table-begin) (org-table-end) :sequence/max-depth true-max))
  (let ((depth-map (sequence/get-table-prop :sequence/depth-hashmap))
        (depth-sets (make-vector (sequence/get-table-prop :sequence/max-depth) '())))
    (maphash (lambda (k v) (aset depth-sets (- v 1) (cons k (aref depth-sets (- v 1))))) depth-map)
    (put-text-property (org-table-begin) (org-table-end) :sequence/depth-sets depth-sets)
    ))


(defun sequence/outputless()
  (let* ((value-map (sequence/get-table-prop :sequence/value-hashmap)))
    (-non-nil (mapcar (lambda (x) (if (sequence/get x :outputs) (sequence/get x :name))) (hash-table-values value-map)))))

(defun sequence/insert-string (str inputs outputs &optional terminal)
  " Insert a possibly new exclusion string into the value-hashmap, updating IOs"
  (message "Inserting string")
  (let* ((value-hashmap (sequence/get-table-prop :sequence/value-hashmap))
         (depth-hashmap (sequence/get-table-prop :sequence/depth-hashmap))
         (current (gethash str value-hashmap `((:name . ,str)
                                               (:inputs . nil )
                                               (:outputs . nil))))
         (updated (sequence/update-IO current inputs outputs)))
    (puthash str updated value-hashmap)
    (cond ((sequence/is-terminal-p str) nil)
          (terminal (progn (sequence/push-table-prop :sequence/terminals str)
                           (puthash str 1 depth-hashmap)))
          ;; not a terminal, so calc depth
          ('t (let ((min-max (sequence/get-depth-range str)))
                (message "Min-Max: %s" min-max)
            ;; if no conflict, set depth
                 (if (>= (- (cdr min-max) (car min-max)) 2)
                     (puthash str (+ 1 (car min-max)) depth-hashmap)
                   ;; if conflicts, need to shuffle a side,
                   ;; and insert a new column
                   (sequence/kahnsort)
                   )
                 )))))

(defun sequence/remove-string (str)
  " Force a string to be completely removed "
  (let ((value-hash (sequence/get-table-prop :sequence/value-hashmap))
        (depth-hash (sequence/get-table-prop :sequence/value-hashmap))
        (terminals (sequence/get-table-prop :sequence/terminals)))

    (remhash str value-hash)
    (remhash str depth-hash)
    (put-text-property (org-table-begin) (org-table-end) :sequence/terminals (remove str terminals))))

(defun sequence/get (obj sym)
  "Get the inputs from an object"
  (cdr (assoc sym obj)))

(defun sequence/del (obj syms)
  "Delete a value from an alist"
  (cond ((not syms) obj)
        ((equal 'symbol (type-of syms))
         (delq (assoc syms obj) obj))
        ((equal 'cons (type-of syms))
         (sequence/del (delq (assoc (car syms) obj) obj) (cdr syms)))))

(defun sequence/update-IO (obj &optional i o)
  " Given an object from the value-hashmap, increment its count "
  (let* ((inputs (sequence/get obj :inputs))
         (outputs (sequence/get obj :outputs))
         (rest (sequence/del obj '(:inputs :outputs))))
    (push `(:inputs . ,(union i inputs)) rest)
    (push `(:outputs . ,(union o outputs)) rest)
    rest))

(defun sequence/get-depth-range (str)
  "Get the (non-inclusive) range of depths available for a value"
  (message "Get Depth Range")
  (if (sequence/is-terminal-p str)
      '(1 . 1)
    (let* ((depth-hash (sequence/get-table-prop :sequence/depth-hashmap))
           (value-hash (sequence/get-table-prop :sequence/value-hashmap))
           (obj (gethash str value-hash))
           (lookup-f (lambda (xs default)
                       (if xs
                           (mapcar (lambda (x) (if (sequence/is-terminal-p x) default (gethash x depth-hash default))) xs)
                         `(,default)))))
      (if obj
          ;; exists, so get range
          (let ((max-input (-max (apply lookup-f `(,(sequence/get obj :inputs) 1))))
                (min-output (-min (apply lookup-f `(,(sequence/get obj :outputs) 100)))))
            `(,max-input . ,min-output))
        ;; doesn't exist, so default range:
        `(1 . 100)))))

(defun sequence/kahnsort ()
  " Run Kahnsort on the graph to determine depths "
  (let* ((value-hash (sequence/get-table-prop :sequence/value-hashmap))
         (terminals (sequence/get-table-prop :sequence/terminals))
         (inputless (sequence/inputless))
         (result (kahnsort value-hash (union terminals inputless) terminals))
         ;; having sorted, get components of result:
         (sorted (car result))
         (active (cadr result))
         (undiscovered (caddr result))
         (max_depth (max (sequence/get-table-prop :sequence/max-depth) (-max (mapcar (lambda (x) (cdr x)) sorted)))))
    ;; add / remove columns as necessary
    ;; update columns
    ;; (debug)
    ))

(defun sequence/update-column (col vals)
  " Given a column and a list of values, set that column "
  ;;goto start column, row 1
  (save-excursion
    (goto-char (org-table-begin))
    (while (and (org-at-table-p) (not (org-at-table-hline-p)))
      (forward-line))
    (forward-line)
    ;; down below the first hline
    ;;loop over values, inserting them
    (loop for x in vals do
          ;; extend the table if necessary
          (if (not (org-at-table-p))
              (progn (forward-line -1)
                     (org-table-insert-row 1)))
          ;; insert the value
          (org-table-put (org-table-current-dline) col x)
          (forward-line)
          ;; skip over hlines
          (if (org-at-table-hline-p)
              (forward-line)))
    ;; clear obsolete values
    (while (org-at-table-p)
      (if (org-at-table-hline-p)
          (forward-line)
        (progn (org-table-goto-column col)
               (org-table-blank-field)
               (forward-line)
               )))
    (org-table-align)))

(defun sequence/redraw-entire-table ()
  " Redraw the entire table"
  (message "Redrawing")
  (let ((cols org-table-current-ncol)
        (needed (sequence/get-table-prop :sequence/max-depth))
        (depth-sets (sequence/get-table-prop :sequence/depth-sets))
        (count 0))
    ;; Add more columns
    (while (< cols needed)
      (org-table-insert-column)
      (cl-incf cols)
      )

    (while (< count cols)
          (message "Updating column: %s" count)
          (if (and (< 1 count) (< count cols))
              (org-table-put 1 count (format "Depth: %s" count)))
          (sequence/update-column (+ count 1) (aref depth-sets count))
          (cl-incf count))
    (sequence/update-column cols (aref depth-sets 0))
    ))

(defun sequence/goto-position (row col)
  " In a table, go to the Column, Row specified """
  (interactive "N\nN")
  ;; Row first, then column, as goto-line resets column otherwise
  (org-table-goto-line row)
  (org-table-goto-column col))

;;----------------------------------------
(defun sequence/new-table ()
  " Create a new table, after having moved to empty space "
  (interactive)
  (if (org-at-table-p)
      (progn (goto-char (org-table-end))
             (insert "\n")))
  ;; add default table
  (insert "\n")
  (beginning-of-line)
  (insert "    | Input Terminals | Non-Terminals | Output Terminals |\n")
  (insert "    |-----------------+---------------+------------------|\n")
  (insert "    |                 |               |                  |\n")

  ;; move point to start of table
  (forward-line -1)
  (goto-char (org-table-begin))
  (sequence/goto-position 2 2)

  (put-text-property (org-table-begin) (org-table-end) :sequence/terminals '())
  (put-text-property (org-table-begin) (org-table-end) :sequence/depth-hashmap (make-hash-table :test 'equal))
  (put-text-property (org-table-begin) (org-table-end) :sequence/max-depth 3)
  (put-text-property (org-table-begin) (org-table-end) :sequence/value-hashmap (make-hash-table :test 'equal))
  (put-text-property (org-table-begin) (org-table-end) :sequence/depth-sets '())
  )

(defun sequence/set-tab (col data)
  " Set the left or right table to data "
  ;; default to left
  ;; retrieve data
  ;; add it to a temporary buffer

  ;; open window on side

  )

(defun sequence/insert-rule (input)
  " Insert an LHS, RHS and optional rule name into the graph "
  (interactive "M")
  (message "Inserting rule")
  (let ((row (org-table-current-line))
        (col (org-table-current-column))
        (parts (split-string input " " 't))
        (on_lhs 't)
        (lhs '())
        (rhs '()))
    (while parts
      (let ((curr (pop parts)))
        (cond ((equal curr "->") (setq on_lhs nil))
              (on_lhs (push curr lhs))
              ('t (push curr rhs)))))

    ;; have got lhs and rhs, add them
    (loop for x in lhs do
          (sequence/insert-string x nil rhs))
    (loop for x in rhs do
          (sequence/insert-string x lhs nil))
    (sequence/analyze-table)
    ;; update table
    (sequence/redraw-entire-table)
    ;; (goto-char (org-table-begin))
    (sequence/goto-position row col)
    (if (get-buffer-window "*Seq Info*")
        (sequence/redraw-inspector))
    ))

(defun sequence/inspect-table ()
  " Open the right tab buffer, displaying details of the selected field "
  ;; create a tab that updates with details
  ;; on current field
  (interactive)
  (let ((wind (get-buffer-window "*Seq Info*")))
    (if wind
      ;; if exists, lose it
        (delete-window wind)
      ;;otherwise create it
      (sequence/redraw-inspector))))

(defun sequence/redraw-inspector ()
      (let ((action '(display-buffer-in-side-window (side . left)))
            (value-hash (sequence/get-table-prop :sequence/value-hashmap))
            (depth-hash (sequence/get-table-prop :sequence/depth-hashmap))
            (terminals (sequence/get-table-prop :sequence/terminals))
            )
        (with-temp-buffer-window "*Seq Info*" action nil
                                 (if (hash-table-p value-hash)
                                     (progn
                                       (princ "Values: \n")
                                       (maphash (lambda (k v) (princ (format "%s :\n\tInputs: %s\n\tOutputs: %s\n\n" k
                                                                             (sequence/get v :inputs) (sequence/get v :outputs))))
                                                value-hash)))
                                 (if (hash-table-p depth-hash)
                                     (progn
                                       (princ "\nDepths: \n")
                                       (maphash (lambda (k v) (princ (format "%s : %s\n" k v))) depth-hash)))
                                 (if terminals
                                     (progn
                                       (princ "\nTerminals: \n")
                                       (mapcar (lambda (k) (princ (format "%s\n" k))) terminals)))
                                 )
        ))

(defun sequence/rename-column ()
  " Rename the current column. Persistently across columm additions and removals "
  ;; get column data row 1

  ;; get new name

  ;; update data

  )

(defun sequence/insert-terminal (term)
  " Insert a terminal value into the graph "
  ;; get value
  (interactive "M")
  (let ((row (org-table-current-line))
        (col (org-table-current-column)))
    ;; insert into data
    (sequence/insert-string term '() '() 't)
    ;; insert into table
    (sequence/analyze-table)
    (sequence/update-column 1 (sequence/get-table-prop :sequence/terminals))
    (sequence/update-column org-table-current-ncol (sequence/get-table-prop :sequence/terminals))
    ;; (goto-char (org-table-begin))
    (sequence/goto-position row col)
    (if (get-buffer-window "*Seq Info*")
        (sequence/redraw-inspector))
    )
  )

(defun sequence/scroll ()
  " Scroll Left or right "
  ;; get direction

  ;; get current indent column

  ;; move in direction

  ;; update indent column

  )

(defun sequence/centre-column ()
  " Centre the current column in the window "
  (interactive)
  (sequence/horizontal-recenter)
  )

(defun sequence/delete-value ()
  " Delete a field from the graph and table "
  ;; Get current field

  ;; remove it from the graph

  ;; update table

  )

(defun sequence/delete-column ()
  " Delete the current column "
  ;; get current column

  ;;get values

  ;;remove from graph

  ;;update table

  )

(defun sequence/merge-column ()
  " Merge the left and right connections of a column, removing the middle "
  ;;get current column

  ;;get values

  ;; connect inputs to outputs

  ;;remove values

  ;; update table

  )

(defun sequence/sort-table ()
  " Sort each column of the table alphabetically or by usage count "
  ;; for each column

  ;; order alphabetically

  )

(defun sequence/highlight ()
  ;; choose colour for inputs / outputs

  ;; get inputs

  ;; get outputs

  ;; find them

  ;; overlay them

  )

;;--------------------
;;definitions
;;--------------------


;;--------------------
;;face definitions
;; use them as symbols 'blah in font-lock-keywords
;;--------------------

;;--------------------
;;Key bindings.
;;use sparse-keymap if only a few bindings
;;--------------------
(defvar-local sequence-mode-map

  (let ((map (make-keymap)))
    (define-key map "TAB" 'org-table-next-field)
    (define-key map "RET" 'org-table-next-row)
    ;;any potential functions? add particular structures?
    ;;TODO: M-RET to prev-line
    map)
  " The basic keymap of the sequence mode. Separate from more complex spacemacs bindings "
)
;; Then register its activation



;;--------------------
;;Keyword based syntax highlighting
;;Specify as a list of (matcher . facename )
;;Potential keywords: operators +-*/!.()""$~ < > != == -> @
;;--------------------

;; highlight exclusion strings

;;--------------------
;;Indentation
;; Potential indent points:
;; newline ending with an EXOP, comma,
;; reset indent if prior line is empty
;;
;;--------------------

;;--------------------
;;Syntax Table
;;--------------------
;;Define the syntax table, for word definitions etc
;;modify-syntax-entry: character, class/flag, syntable)
;;classes/syntax flags: include 'w': word, '.':punctuation,
;; see: C-h ? r elisp manual syntax-tables

;;--------------------
;;Autoloading
;;--------------------
(add-to-list 'auto-mode-alist '("\\.seq\\'" . sequence-mode))

;; --------------------
;;Entry Function
;;--------------------
(define-derived-mode sequence-mode fundamental-mode "Sequence Mode"
  "Major Mode for creating a sequence of rules "
  (interactive)
  (kill-all-local-variables)
  ;; Set the Org Table minor mode
  (orgtbl-mode)
  ;; set table coordinates to show
  (use-local-map sequence-mode-map)
  ;; (set (make-local-variable 'font-lock-defaults) '(sequence-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'sequence-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table sequence-mode-syntax-table)
  (setq major-mode 'sequence-mode)
  (setq mode-name "SEQUENCE")
  ;;(run-mode-hooks 'sequence-mode-hook)
  )

;; TODO: add to hs-special-modes-alist
;; TODO: mod after-change-functions
(provide 'sequence-mode)


;; useful functions:
;; org-table-goto-field
