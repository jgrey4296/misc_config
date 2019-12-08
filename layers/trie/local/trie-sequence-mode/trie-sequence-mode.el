;; A Custom mode adapting org-table

(require 'dash)
(require 'org-table)
(require 'kahnsort)
;; add transient / hydra
;; add helm

;;--------------------
;; Mode Variables
;;--------------------
(defconst trie-sequence/left-tab "Trie-SequenceMLeft")
(defconst trie-sequence/right-tab "Trie-SequenceMRight")
(defconst trie-sequence/info-tab "*Seq Info*")

(defvar trie-sequence/current-colour "green")
(defvar trie-sequence/input-colour "blue")
(defvar trie-sequence/output-colour "orange")

(defvar trie-sequence/overlays '())
(defvar trie-sequence/free-overlays '())
(defvar trie-sequence/overlay-max 20)
(defvar trie-sequence/inspector-overlay nil)
;;--------------------
;; Overlays
;;--------------------
(defun trie-sequence/make-overlay (beg end type)
  (let ((overlay (if trie-sequence/free-overlays (pop trie-sequence/free-overlays) (make-overlay 1 2)))
        (color "green"))
    (if (not (-contains? trie-sequence/overlays overlay))
        (push overlay trie-sequence/overlays))
    (setq color (cond ((equal type :point) trie-sequence/current-colour)
                      ((equal type :input) trie-sequence/input-colour)
                      ((equal type :output) trie-sequence/output-colour)))
    (overlay-put overlay 'face `((foreground-color . ,color)))
    (overlay-put overlay 'font-lock-ignore t)
    (move-overlay overlay beg end)

    ))
(defun trie-sequence/clear-overlays ()
  (loop for x in trie-sequence/overlays do
        (push x trie-sequence/free-overlays)
        (delete-overlay x)
        )
  (setq-local trie-sequence/overlays '())
  )
(defun trie-sequence/set-overlays-for-current ()
  " Set overlays for the currently selected cell "
  (let* ((row (org-table-current-line))
         (col (org-table-current-column))
         (value (substring-no-properties (org-table-get row col)))
         (value-hash (trie-sequence/get-table-prop :trie-sequence/value-hashmap))
         (value-obj (gethash value value-hash))
         ;; Get the inputs and outputs
         (inputs (trie-sequence/get value-obj :inputs))
         (outputs (trie-sequence/get value-obj :outputs))
         (wind (get-buffer-window trie-sequence/info-tab))
         )

    ;;Dealing with side window:
    (if wind
        (with-current-buffer trie-sequence/info-tab
          (goto-char (point-min))
          (search-forward-regexp (format "^*** %s" value))
          (if (not (overlayp trie-sequence/inspector-overlay))
              (progn (setq trie-sequence/inspector-overlay (make-overlay 1 2))
                     (overlay-put trie-sequence/inspector-overlay 'face '((foreground-color . "green")))))
          (move-overlay trie-sequence/inspector-overlay
                        (line-beginning-position)
                        (point)
                        (get-buffer trie-sequence/info-tab))))
    ;;Main window:
    (save-excursion
      (goto-char (org-table-begin))
      (search-forward value)
      (trie-sequence/make-overlay
       (progn (skip-chars-backward "^|") (point))
       (progn (skip-chars-forward "^|") (point))
       :point)

      ;;Overlay on inputs
      (loop for x in inputs do
            (goto-char (org-table-begin))
            (if (trie-sequence/is-terminal-p x)
                (progn (search-forward x)
                       (trie-sequence/make-overlay
                        (progn (skip-chars-backward "^|") (point))
                        (progn (skip-chars-forward "^|") (point))
                        :input)))
            (search-forward x)
            (trie-sequence/make-overlay
             (progn (skip-chars-backward "^|") (point))
             (progn (skip-chars-forward "^|") (point))
             :input))

      ;;Overlay on outputs
      (loop for x in outputs do
            (goto-char (org-table-begin))
            (if (trie-sequence/is-terminal-p x)
                (progn (search-forward x)
                       (trie-sequence/make-overlay
                        (progn (skip-chars-backward "^|") (point))
                        (progn (skip-chars-forward "^|") (point))
                        :output)))
            (search-forward x)
            (trie-sequence/make-overlay
             (progn (skip-chars-backward "^|") (point))
             (progn (skip-chars-forward "^|") (point))
             :output))
      )
    )
  )

;;--------------------
;;Motion
;;--------------------
(defun trie-sequence/user-inc-column (count)
  " Increment the user point by a column,
    Updating the highlights as well "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (let ((curr-table-column (org-table-current-column))
            (max-column org-table-current-ncol)
            (curr-col (current-column))
            (new-col (+ curr-table-column (if count (prefix-numeric-value count) 1)))
            )
        (org-table-goto-column (if (< new-col max-column) new-col curr-table-column))
        ;;if cell is empty, go up until it isnt
        (while (and (org-at-table-p) (not (looking-at "[[:alnum:]-]")))
          (forward-line -1)
          (move-to-column curr-col)
          )
        (if (looking-at "-")
            (progn (forward-line)
                   (move-to-column curr-col))
          )
        ;;Deal with overlays
        (trie-sequence/clear-overlays)
        (trie-sequence/set-overlays-for-current)
        )
    ;;No
    (progn (trie-sequence/clear-overlays)
           (evil-forward-char (prefix-numeric-value count))))
  )
(defun trie-sequence/user-dec-column (count)
  "Decrement the user point by a column "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (let ((curr-table-column (org-table-current-column))
            curr-col
            (new-col (- curr-table-column (if count (prefix-numeric-value count) 1)))
            )
        (org-table-goto-column (if (>= new-col 1) new-col curr-table-column))
        ;;Move to a non empty cell
        (setq curr-col (current-column))
        (while (not (looking-at "[[:alnum:]-]"))
          (forward-line -1)
          (move-to-column curr-col)
          )
        (if (looking-at "-")
            (progn (forward-line)
                   (move-to-column curr-col))
          )
        ;;Deal with overlays
        (trie-sequence/clear-overlays)
        (trie-sequence/set-overlays-for-current)
        )
    ;;No
    (progn (trie-sequence/clear-overlays)
           (evil-backward-char (prefix-numeric-value count))))
  )

(defun trie-sequence/user-dec-line (count)
  "decrement the user point by a line "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (progn (let ((row (org-table-current-line))
                   (col (org-table-current-column)))
               (org-table-goto-line (- row (if count (prefix-numeric-value count) 1)))
               (org-table-goto-column col))
             (trie-sequence/clear-overlays)
             (trie-sequence/set-overlays-for-current)
             )
    ;;No
    (progn (trie-sequence/clear-overlays)
           (evil-previous-line (prefix-numeric-value count))))
  )
(defun trie-sequence/user-inc-line (count)
  "increment the user point by a line "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (progn (let ((row (org-table-current-line))
                   (col (org-table-current-column)))
               (org-table-goto-line (+ row (if count (prefix-numeric-value count) 1)))
               (org-table-goto-column col))
             (trie-sequence/clear-overlays)
             (trie-sequence/set-overlays-for-current)
             )
    ;;No
    (progn (trie-sequence/clear-overlays)
           (evil-next-line (prefix-numeric-value count))))
  )

(defun trie-sequence/centre-column ()
  " Centre the current column in the window "
  (interactive)
  (trie-sequence/horizontal-recenter)
  )
(defun trie-sequence/horizontal-recenter ()
  " Force horizontal recenter, based on:
https://stackoverflow.com/questions/1249497 "
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))

(defun trie-sequence/goto-position (row col)
  " In a table, go to the Column, Row specified """
  (interactive "N\nN")
  ;; Row first, then column, as goto-line resets column otherwise
  (org-table-goto-line row)
  (org-table-goto-column col))

(defun trie-sequence/scroll ()
  " Scroll Left or right "
  ;; get direction

  ;; get current indent column

  ;; move in direction

  ;; update indent column

  )

;;--------------------
;; Utilities
;;--------------------
(defun trie-sequence/get-table-prop (sym)
  " Get a property from the current table "
  (get-text-property (org-table-begin) sym))
(defun trie-sequence/push-table-prop (sym val)
  "Push a value to a list property of the current table"
  (let ((lst (get-text-property (org-table-begin) sym)))
    (push val lst)
    (put-text-property (org-table-begin) (org-table-end) sym lst)))

(defun trie-sequence/is-terminal-p (str)
  (-contains? (trie-sequence/get-table-prop :trie-sequence/terminals) str))

(defun trie-sequence/inputless ()
  (let* ((value-map (trie-sequence/get-table-prop :trie-sequence/value-hashmap)))
    (-non-nil (mapcar (lambda (x) (if (trie-sequence/get x :inputs)
                                      (trie-sequence/get x :name)))
                      (hash-table-values value-map)))))
(defun trie-sequence/outputless ()
  (let* ((value-map (trie-sequence/get-table-prop :trie-sequence/value-hashmap)))
    (-non-nil (mapcar (lambda (x) (if (trie-sequence/get x :outputs)
                                      (trie-sequence/get x :name)))
                      (hash-table-values value-map)))))

(defun trie-sequence/analyze-table ()
  " Update analytics on the table "
  (message "Analyzing table")
  (org-table-analyze)
  ;; create column sets
  (let* ((depth-hash (trie-sequence/get-table-prop :trie-sequence/depth-hashmap))
         (dh_max (if (hash-table-empty-p depth-hash) 0 (-max (hash-table-values depth-hash))))
         (true-max (max 3 (+ 1 dh_max))))
    (put-text-property (org-table-begin) (org-table-end) :trie-sequence/max-depth true-max))
  (let ((depth-map (trie-sequence/get-table-prop :trie-sequence/depth-hashmap))
        (depth-sets (make-vector (trie-sequence/get-table-prop :trie-sequence/max-depth) '())))
    (maphash (lambda (k v) (aset depth-sets (- v 1) (cons k (aref depth-sets (- v 1))))) depth-map)
    (put-text-property (org-table-begin) (org-table-end) :trie-sequence/depth-sets depth-sets)
    ))

(defun trie-sequence/insert-string (str inputs outputs &optional terminal)
  " Insert a possibly new exclusion string into the value-hashmap, updating IOs"
  (message "Inserting string")
  (let* ((value-hashmap (trie-sequence/get-table-prop :trie-sequence/value-hashmap))
         (depth-hashmap (trie-sequence/get-table-prop :trie-sequence/depth-hashmap))
         (current (gethash str value-hashmap `((:name . ,str)
                                               (:inputs . nil )
                                               (:outputs . nil))))
         (updated (trie-sequence/update-IO current inputs outputs)))
    (puthash str updated value-hashmap)
    ;;Handle graph ordering:
    (cond ((trie-sequence/is-terminal-p str) nil) ;;already a terminal
          ;;New terminal
          (terminal (progn (trie-sequence/push-table-prop :trie-sequence/terminals str)
                           (puthash str 1 depth-hashmap)))
          ;; not a terminal, so calc depth
          (t (let ((min-max (trie-sequence/get-depth-range str)))
               (message "Min-Max: %s" min-max)
               ;; if no conflict, set depth
               (if (>= (- (cdr min-max) (car min-max)) 2)
                   (puthash str (+ 1 (car min-max)) depth-hashmap)
                 ;; if conflicts, need to shuffle a side,
                 ;; and insert a new column
                 (trie-sequence/kahnsort)
                 )
               )))))
(defun trie-sequence/remove-string (str)
  " Force a string to be completely removed "
  (let* ((value-hash (trie-sequence/get-table-prop :trie-sequence/value-hashmap))
         (depth-hash (trie-sequence/get-table-prop :trie-sequence/depth-hashmap))
         (terminals (trie-sequence/get-table-prop :trie-sequence/terminals))
         (updated-terminals (remove str terminals))
         )
    (message "Removing %s from %s" str terminals)
    (message "Terminals: %s" updated-terminals)
    (remhash str value-hash)
    (remhash str depth-hash)
    (put-text-property (org-table-begin) (org-table-end) :trie-sequence/terminals updated-terminals)
    ))

(defun trie-sequence/get (obj sym)
  "Get the inputs from an object"
  (cdr (assoc sym obj)))
(defun trie-sequence/del (obj syms)
  "Delete a value from an alist"
  (cond ((not syms) obj)
        ((equal 'symbol (type-of syms))
         (delq (assoc syms obj) obj))
        ((equal 'cons (type-of syms))
         (trie-sequence/del (delq (assoc (car syms) obj) obj) (cdr syms)))))

(defun trie-sequence/update-IO (obj &optional i o)
  " Given an object from the value-hashmap, increment its count "
  (let* ((inputs (trie-sequence/get obj :inputs))
         (outputs (trie-sequence/get obj :outputs))
         (rest (trie-sequence/del obj '(:inputs :outputs))))
    (push `(:inputs . ,(union i inputs)) rest)
    (push `(:outputs . ,(union o outputs)) rest)
    rest))

(defun trie-sequence/get-depth-range (str)
  "Get the (non-inclusive) range of depths available for a value"
  (message "Get Depth Range")
  (if (trie-sequence/is-terminal-p str)
      '(1 . 1)
    (let* ((depth-hash (trie-sequence/get-table-prop :trie-sequence/depth-hashmap))
           (value-hash (trie-sequence/get-table-prop :trie-sequence/value-hashmap))
           (obj (gethash str value-hash))
           (lookup-f (lambda (xs default)
                       (if xs
                           (mapcar (lambda (x) (if (trie-sequence/is-terminal-p x) default (gethash x depth-hash default))) xs)
                         `(,default)))))
      (if obj
          ;; exists, so get range
          (let ((max-input (-max (apply lookup-f `(,(trie-sequence/get obj :inputs) 1))))
                (min-output (-min (apply lookup-f `(,(trie-sequence/get obj :outputs) 100)))))
            `(,max-input . ,min-output))
        ;; doesn't exist, so default range:
        `(1 . 100)))))
(defun trie-sequence/kahnsort ()
  " Run Kahnsort on the graph to determine depths "
  (let* ((value-hash (trie-sequence/get-table-prop :trie-sequence/value-hashmap))
         (terminals (trie-sequence/get-table-prop :trie-sequence/terminals))
         (inputless (trie-sequence/inputless))
         (result (kahnsort value-hash (union terminals inputless) terminals))
         ;; having sorted, get components of result:
         (sorted (car result))
         (active (cadr result))
         (undiscovered (caddr result))
         (max_depth (max (trie-sequence/get-table-prop :trie-sequence/max-depth) (-max (mapcar (lambda (x) (cdr x)) sorted)))))
    ;; add / remove columns as necessary
    ;; update columns
    ;; (debug)
    ))

;;--------------------
;;Drawing
;;--------------------
(defun trie-sequence/update-column (col vals)
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
(defun trie-sequence/redraw-entire-table ()
  " Redraw the entire table"
  (message "Redrawing")
  (let ((cols org-table-current-ncol)
        (needed (trie-sequence/get-table-prop :trie-sequence/max-depth))
        (depth-sets (trie-sequence/get-table-prop :trie-sequence/depth-sets))
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
      (trie-sequence/update-column (+ count 1) (aref depth-sets count))
      (cl-incf count))
    (trie-sequence/update-column cols (aref depth-sets 0))
    ))
(defun trie-sequence/redraw-inspector ()
  (let ((action '(display-buffer-in-side-window (side . left)))
        (value-hash (trie-sequence/get-table-prop :trie-sequence/value-hashmap))
        (depth-hash (trie-sequence/get-table-prop :trie-sequence/depth-hashmap))
        (terminals (seq-copy (trie-sequence/get-table-prop :trie-sequence/terminals)))
        )
    (with-temp-buffer-window trie-sequence/info-tab action nil
                             (princ "* Sequence Information\n")
                             (if (hash-table-p value-hash)
                                 (progn
                                   (princ "** Values: \n")
                                   (mapc (lambda (k) (princ (format "*** %s :\n\tInputs: %s\n\tOutputs: %s\n\n" k
                                                                    (trie-sequence/get (gethash k value-hash) :inputs)
                                                                    (trie-sequence/get (gethash k value-hash) :outputs))))
                                         (sort (hash-table-keys value-hash) (lambda (x y) (string-lessp (downcase x) (downcase y)))))))
                             (if (hash-table-p depth-hash)
                                 (progn
                                   (princ "** Depths: \n")
                                   (mapc (lambda (k) (princ (format "   %s : %s\n" k (gethash k depth-hash))))
                                         (sort (hash-table-keys depth-hash) (lambda (x y) (string-lessp (downcase x) (downcase y)))))))
                             (if terminals
                                 (progn
                                   (princ "\n** Terminals: \n")
                                   (mapc (lambda (k) (princ (format "   %s\n" k))) (sort terminals (lambda (x y) (string-lessp (downcase x) (downcase y)))))))
                             )
    (with-current-buffer trie-sequence/info-tab
      (org-mode)
      (org-show-all)
      )
    ))
(defun trie-sequence/inspect-table ()
  " Open the right tab buffer, displaying details of the selected field "
  ;; create a tab that updates with details
  ;; on current field
  (interactive)
  (let ((wind (get-buffer-window trie-sequence/info-tab)))
    (if wind
        ;; if exists, lose it
        (delete-window wind)
      ;;otherwise create it
      (trie-sequence/redraw-inspector))))

(defun trie-sequence/highlight ()
  ;; choose colour for inputs / outputs

  ;; get inputs

  ;; get outputs

  ;; find them

  ;; overlay them

  )
;;--------------------
;;Table Operations
;;--------------------
(defun trie-sequence/new-table ()
  " Create a new table, after having moved to empty space "
  (interactive)
  (if (org-at-table-p)
      (progn (goto-char (org-table-end))
             (insert "\n")))
  ;; add default table
  (let ((input (propertize " Input Terminals " :trie-sequence/terminal t))
        (output (propertize " Output Terminals " :trie-sequence/terminal t)))
    (insert "\n")
    (beginning-of-line)
    (insert (format "    |%s| Non-Terminals |%s|\n" input output))
    (insert "    |-----------------+---------------+------------------|\n")
    (insert "    |                 |               |                  |\n")
    )
  ;; move point to start of table
  (forward-line -1)
  (goto-char (org-table-begin))
  (trie-sequence/goto-position 2 2)

  (put-text-property (org-table-begin) (org-table-end) :trie-sequence/terminals '())
  (put-text-property (org-table-begin) (org-table-end) :trie-sequence/depth-hashmap (make-hash-table :test 'equal))
  (put-text-property (org-table-begin) (org-table-end) :trie-sequence/max-depth 3)
  (put-text-property (org-table-begin) (org-table-end) :trie-sequence/value-hashmap (make-hash-table :test 'equal))
  (put-text-property (org-table-begin) (org-table-end) :trie-sequence/depth-sets '())
  )
(defun trie-sequence/insert-rule (input)
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
          (trie-sequence/insert-string x nil rhs))
    (loop for x in rhs do
          (trie-sequence/insert-string x lhs nil))
    (trie-sequence/analyze-table)
    ;; update table
    (trie-sequence/redraw-entire-table)
    ;; (goto-char (org-table-begin))
    (trie-sequence/goto-position row col)
    (if (get-buffer-window trie-sequence/info-tab)
        (trie-sequence/redraw-inspector))
    ))
(defun trie-sequence/insert-terminal (term)
  " Insert a terminal value into the graph "
  ;; get value
  (interactive "M")
  (let ((row (org-table-current-line))
        (col (org-table-current-column)))
    ;; insert into data
    (trie-sequence/insert-string term '() '() t)
    ;; insert into table
    (trie-sequence/analyze-table)
    (trie-sequence/update-column 1 (trie-sequence/get-table-prop :trie-sequence/terminals))
    (trie-sequence/update-column org-table-current-ncol (trie-sequence/get-table-prop :trie-sequence/terminals))
    ;; (goto-char (org-table-begin))
    (trie-sequence/goto-position row col)
    (if (get-buffer-window trie-sequence/info-tab)
        (trie-sequence/redraw-inspector))
    )
  )

(defun trie-sequence/delete-value ()
  " Delete a field from the graph and table "
  (interactive)
  (let* ((curr-line (org-table-current-line))
         (curr-col (org-table-current-column))
         (curr-value (org-table-get curr-line curr-col))
         )
    ;; remove it from the graph
    ;;todo: remove from inputs and outputs
    (trie-sequence/remove-string curr-value)

    ;; update table
    (org-table-put curr-line curr-col "" t)
    (trie-sequence/goto-position curr-line curr-col)
    )
  )
(defun trie-sequence/delete-column ()
  " Delete the current column "
  (interactive)
  (let ((curr-column (org-table-current-column)))
    (org-table-goto-line 2)
    (org-table-goto-column curr-column)
    (while (org-at-table-p)
      (trie-sequence/delete-value)
      (forward-line)
      (org-table-goto-column curr-column))
    (forward-line -1)
    (trie-sequence/goto-position 2 curr-column)
    )
  )

(defun trie-sequence/rename-column ()
  " Rename the current column. Persistently across columm additions and removals "
  (interactive)
  ;; get column data row 1
  (if (org-at-table-p)
      (let* ((curr-table-row (org-table-current-line))
             (curr-table-col (org-table-current-column))
             (curr-name (org-table-get 1 curr-table-col))
             (is-terminal (get-text-property 0 :trie-sequence/terminal curr-name))
             new-name
             )
        (if (not (or (string-empty-p curr-name) is-terminal))
            (progn (trie-sequence/goto-position 1 curr-table-col)
                   (setq new-name (read-string "New Column Name: "))
                   (org-table-put 1 curr-table-col new-name t)
                   (trie-sequence/goto-position curr-table-row curr-table-col)
                   )
          )
        )
    )
  )
(defun trie-sequence/merge-column ()
  " Merge the left and right connections of a column, removing the middle "
  ;;get current column

  ;;get values

  ;; connect inputs to outputs

  ;;remove values

  ;; update table

  )
(defun trie-sequence/sort-table ()
  " Sort each column of the table alphabetically or by usage count "
  (interactive)
  (let ((max-column org-table-current-ncol)
        (curr-table-column (org-table-current-column))
        (curr-table-line (org-table-current-line))
        (curr-col 1)
        (column-values (trie-sequence/get-table-prop :trie-sequence/depth-sets))
        (curr-count 0)
        curr-values
        )
    (trie-sequence/goto-position 2 1)
    (while (< (org-table-current-column) max-column)
      ;;Get the values
      (setq curr-values (sort (seq-copy (aref column-values curr-count))
                              (lambda (x y) (string-lessp (downcase x) (downcase y)))))
      ;;set the columns values
      (trie-sequence/update-column curr-col curr-values)
      (incf curr-col)
      (incf curr-count)
      (trie-sequence/goto-position 2 curr-col)
      )
    (setq curr-values (sort (seq-copy (aref column-values 0))
                            (lambda (x y) (string-lessp (downcase x) (downcase y)))))
    (trie-sequence/update-column (- curr-col 1) curr-values)
    (trie-sequence/goto-position curr-table-line curr-table-column)
    )
  )
;;--------------------
;;Tabs
;;--------------------
(defun trie-sequence/set-tab (col data)
  " Set the left or right table to data "
  ;; default to left
  ;; retrieve data
  ;; add it to a temporary buffer

  ;; open window on side

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
(defvar-local trie-sequence-mode-map

  (let ((map (make-keymap)))
    (define-key map "TAB" 'org-table-next-field)
    (define-key map "RET" 'org-table-next-row)
    ;;any potential functions? add particular structures?
    ;;TODO: M-RET to prev-line
    map)
  " The basic keymap of the trie-sequence mode. Separate from more complex spacemacs bindings "
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
(add-to-list 'auto-mode-alist '("\\.seq\\'" . trie-sequence-mode))

;; --------------------
;;Entry Function
;;--------------------
(define-derived-mode trie-sequence-mode fundamental-mode "Trie-Sequence Mode"
  "Major Mode for creating a trie-sequence of rules "
  (interactive)
  (kill-all-local-variables)
  ;; Set the Org Table minor mode
  (orgtbl-mode)
  ;; set table coordinates to show
  (use-local-map trie-sequence-mode-map)
  ;; (set (make-local-variable 'font-lock-defaults) '(trie-sequence-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'trie-sequence-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table trie-sequence-mode-syntax-table)
  (setq major-mode 'trie-sequence-mode
        mode-name "TRIE-SEQUENCE")
  ;;(run-mode-hooks 'trie-sequence-mode-hook)
  )

;; TODO: add to hs-special-modes-alist
;; TODO: mod after-change-functions
(provide 'trie-sequence-mode)

;; useful functions:
;; org-table-goto-field
