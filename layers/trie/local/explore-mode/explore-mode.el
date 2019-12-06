(require 'dash)
(require 'tree-gen)

(defgroup explore-mode '() "Trie Exploration Mode Customizations")

(defstruct explore/tree-data name
           root
           (indents '())
           (curr-path '())
           (max-lines 0)
           (start-pos (make-marker))
           (path-pos (make-marker))
           (overlays (make-hash-table)))

(defvar-local explore/current-markers (list (make-marker) (make-marker)))
(defvar-local explore/next-heading (make-marker))
(defvar-local explore/current-layer 0)
(defvar-local explore/overlay-max 20)
(defvar-local explore/current-data nil)
;;--------------------------------------------------
;; Overlays
(defun explore/make-overlay (beg end layer)
  """ Create or reuse an overlay to use for a particular layer in the tree """
  (let ((overlay (or (gethash layer (explore/tree-data-overlays explore/current-data))
                     (puthash layer (make-overlay 1 2) (explore/tree-data-overlays explore/current-data))))
        (color "green"))
    (overlay-put overlay 'face `((foreground-color . ,color)))
    (move-overlay overlay beg (- end 3))
    )
  )
(defun explore/clear-overlays ()
  """ Clear all overlays """
  (loop for x in (hash-table-values (explore/tree-data-overlays explore/current-data)) do
        (delete-overlay x)
        )
  (clrhash (explore/tree-data-overlays explore/current-data))
  )
;;--------------------------------------------------
;; Utils

(defmacro explore/pop-list-to-length (lst n)
  """ Pop values off a list IN PLACE until it is a given length,
then return its new head """
  `(progn (while (and ,lst (> (length ,lst) ,n))
            (pop ,lst)
            )
          (car ,lst))
  )
(defmacro explore/reduce-list-length (lst n)
  """ Pop values off a DUPLICATED list until it is a given length,
then return the modified list """
  `(let* ((the_list (seq-copy ,lst)))
     (while (and the_list (> (length the_list) ,n))
       (pop the_list)
       )
     the_list)
  )

(defun explore/cols-to-bounds (seq col &optional maxlen)
  """ Given a sequence of indents and a column value,
calculate the bounds that column falls within """
  (if (not maxlen) (setq maxlen 20))
  (let* ((indents (copy-seq seq))
         (upper 0)
         (lower 0)
         )
    (if (>= col (car indents))
        (setq lower (car indents)
              upper (+ lower maxlen))
      (while (and indents (< col (car indents)))
        (setq upper (pop indents)
              lower (or (car indents) 0))
        )
      )
    ;; (message "Upper: %s Lower: %s" upper lower)
    (list upper lower)
    )
  )
(defun explore/cols-to-pos (bounds)
  """ Convert at least a pair of bounding columns to actual buffer positions """
  (let* ((lower (cadr bounds))
         (upper (car bounds))
         )
    (list (explore/col-to-pos upper) (explore/col-to-pos lower))
    )
  )
(defun explore/col-to-pos (col)
  """ Convert a column value to an actual buffer position """
  (save-excursion
    (move-to-column col t)
    (point))
  )
(defun explore/col-to-layer (col)
  """ Convert a column value to the corresponding layer of the tree """
  (let* ((indents (reverse (copy-seq (explore/tree-data-indents explore/current-data))))
         (cal-layer 0)
         )
    (while (and indents (>= col (car indents)))
      (pop indents)
      (incf cal-layer)
      )
    ;; (message "Col to Layer: %s -> %s" col cal-layer)
    cal-layer
    )
  )

(defun explore/print-state ()
  (let* ((data (save-excursion
                 (outline-previous-heading)
                 (get-text-property (point) :tree-data)))
         (indents (explore/tree-data-indents data))
         (path (explore/tree-data-curr-path data))
         (maxlines (explore/tree-data-max-lines data))
         (start-pos (marker-position (explore/tree-data-start-pos data)))
         (path-pos (marker-position (explore/tree-data-path-pos data)))
         )
    (message "\nState:\nIndents: %s\nPath: %s\nMax Lines: %s\nStart Pos: %s\nPath Pos: %s\n\n"
             indents path maxlines start-pos path-pos)
    )
  )
;;--------------------------------------------------
;; Drawing
(defun explore/draw-children ()
  (let* ((rev-path (reverse (explore/tree-data-curr-path explore/current-data)))
         (node (explore/tree-get (explore/tree-data-root explore/current-data) rev-path))
         (layer (+ 1 (length rev-path)))
         )
     (explore/draw-children-safe node layer))
  )
(defun explore/draw-children-safe (node layer)
  (let* ((children (explore/node-children node))
         (num_children (length (hash-table-values children)))
         (maxlen (if (hash-table-empty-p children) 20
                   (apply 'max (mapcar (lambda (x) (length x))
                                       (hash-table-keys children)))))
         (indent_lower (car (explore/tree-data-indents explore/current-data)))
         (indent_upper (+ indent_lower maxlen))
         )
    (if (hash-table-empty-p children) (message "HASH empty"))
    ;;move to the indent head position
    (goto-char (marker-position (explore/tree-data-start-pos explore/current-data)))
    (set-marker explore/next-heading (save-excursion (outline-next-visible-heading 1) (point)))
    (seq-each (lambda (x)
                (move-to-column indent_lower t)
                (delete-region (point) (line-end-position))
                ;;TODO: possibly limit size of x
                (insert x)
                (put-text-property (explore/col-to-pos indent_lower)
                                   (explore/col-to-pos indent_upper)
                                   :layer layer)
                (move-to-column (+ indent_lower maxlen) t)
                (insert " : ")
                ;; Move forward a line if there is one already, otherwise add one
                (forward-line 1)
                (if (<= (marker-position explore/next-heading) (point))
                    (progn (forward-line -1) (goto-char (line-end-position)) (newline)))
                ) (sort (hash-table-keys children) (lambda (x y) (string-lessp (downcase x) (downcase y)))))
    ;; Clear any remaining lines
    (if (> (explore/tree-data-max-lines explore/current-data) num_children)
        (let ((clear-lines (make-list (- (explore/tree-data-max-lines explore/current-data) num_children) nil)))
          (message "Clearing %s lines" (length clear-lines))
          (while (< (point) (marker-position explore/next-heading))
            (move-to-column indent_lower t)
            (delete-region (point) (line-end-position))
            (forward-line 1)
            )
          )
      )
    )
  nil
  )
(defun explore/draw-path ()
  """ Draw the path through the tree currently being used """
  (save-excursion
    (goto-char (marker-position (explore/tree-data-path-pos explore/current-data)))
    (delete-region (point) (line-end-position))
    (seq-each (lambda (pair)
                (move-to-column (car pair) t)
                (insert (cdr pair)))
              (-zip-pair (reverse (explore/tree-data-indents explore/current-data))
                         (reverse (explore/tree-data-curr-path explore/current-data))))
    (add-text-properties (marker-position (explore/tree-data-path-pos explore/current-data)) (line-end-position)
                         '(font-lock-ignore t))
    (add-face-text-property (marker-position (explore/tree-data-path-pos explore/current-data)) (line-end-position)
                            '(:foreground "red"))
    )
  )

;;--------------------------------------------------
;; Interaction

(defun explore/update-tree-data()
  (interactive)
  ;;Use correct tree data:
  (save-excursion
    (outline-previous-heading)
    (setq explore/current-data (get-text-property (point) :tree-data))
    )
  ;;Calculate and update:
  (let* ((current-indent (current-column))

         (layer (explore/col-to-layer current-indent))

         (short-path (explore/reduce-list-length (explore/tree-data-curr-path explore/current-data) (- layer 1)))

         (parent (explore/tree-get (explore/tree-data-root explore/current-data) (reverse short-path)))
         (pchildren (explore/node-children parent))
         (pmaxlen (if (hash-table-empty-p pchildren) 20
                    (apply 'max (mapcar (lambda (x) (length x)) (hash-table-keys pchildren)))))

         (bounds (explore/cols-to-bounds (explore/tree-data-indents explore/current-data) current-indent (+ pmaxlen 3)))
         (positions (explore/cols-to-pos bounds))

         (substr (if (< 0 layer) (string-trim (buffer-substring (cadr positions) (- (car positions) 3))) nil))

         (new-path (if (< 0 layer) (cons substr short-path) '()))

         ;;node inspection
         (node (explore/tree-get (explore/tree-data-root explore/current-data) (reverse new-path)))
         (children (explore/node-children node))
         (num_children (length (hash-table-values children)))
         (max-children (max (explore/tree-data-max-lines explore/current-data) num_children))
         (indent_upper (car bounds))
         )

    (if (or (not (string-empty-p substr)) (eq 0 layer))
        (progn
          ;;update tree data
          (setf (explore/tree-data-curr-path explore/current-data) new-path
                (explore/tree-data-max-lines explore/current-data) max-children)
          (explore/pop-list-to-length (explore/tree-data-indents explore/current-data) layer)
          (push indent_upper (explore/tree-data-indents explore/current-data))
          ;; (explore/print-state)
          (set-marker (car explore/current-markers) (cadr positions))
          (set-marker (cadr explore/current-markers) (car positions))
          (setq explore/current-layer layer)
          )
      (progn
        (set-marker (car explore/current-markers) (point-min))
        (set-marker (cadr explore/current-markers) (point-min))
        (setq explore/current-layer 0)
        )
      )
    (message "\n-----\nDATA:\nPath: %s\nIndents: %s\nLayer: %s\n"
             (explore/tree-data-curr-path explore/current-data)
             (explore/tree-data-indents explore/current-data)
             explore/current-layer)
    )
  )
(defun explore/expand-entry ()
  """ Expand the node the user selects """
  (interactive)
  (explore/update-tree-data)
  (save-excursion
    (explore/draw-children)
    (explore/draw-path)
    ;;todo: make overlay
    )
  (if (> explore/current-layer 0)
      (explore/make-overlay (marker-position (car explore/current-markers))
                            (marker-position (cadr explore/current-markers))
                            explore/current-layer)
    (explore/clear-overlays)
    )
  )

(defun explore/insert-entry ()
  """ insert a new node into the tree from insert state"""
  (interactive)
  (explore/update-tree-data)
  (let* ((curr-data explore/current-data)
         (parent-path (explore/tree-data-curr-path curr-data))
         (node (explore/tree-get (explore/tree-data-root curr-data) (reverse parent-path)))
         (bounds (explore/cols-to-pos (explore/cols-to-bounds (explore/tree-data-indents curr-data) (current-column))))
         (substr (string-trim (buffer-substring-no-properties (cadr bounds) (point))))
         )
    (if (not (eq explore/current-layer (length parent-path)))
        (message "Path / Layer Mismatch")
      (progn
        (explore/node-add-child node substr)
        (save-excursion
          (move-to-column (- (current-column) (length substr) 3))
          (explore/expand-entry)
          )
        )
      )
    )
  )
(defun explore/insert-at-leaf ()
  " Insert from minibuffer "
  (interactive)
  (save-excursion
    (outline-previous-heading)
    (setq explore/current-data (get-text-property (point) :tree-data))
    )
  (let* ((value (read-string "Value: "))
         (parent-path (explore/tree-data-curr-path explore/current-data))
         (node (explore/tree-get (explore/tree-data-root explore/current-data) (reverse parent-path)))
         )
    (explore/node-add-child node value)
    )
  (explore/expand-entry)
  )

(defun explore/delete-entry ()
  (interactive)
  (explore/update-tree-data)
  ;; get the substring
  (let ((to-delete (car (explore/tree-data-curr-path explore/current-data)))
        (path (cdr (explore/tree-data-curr-path explore/current-data))))
    (explore/tree-remove (explore/tree-data-root explore/current-data)
                         (reverse path) to-delete)
    (save-excursion
      (explore/layer-decrease)
      (explore/expand-entry)
      )
    )
  )

(defun explore/layer-decrease ()
  (interactive)
  (save-excursion
    (outline-previous-heading)
    (setq explore/current-data (get-text-property (point) :tree-data))
    )
  (let ((indents (seq-copy (explore/tree-data-indents explore/current-data)))
        (curr-point (current-column))
        (start-pos (explore/tree-data-start-pos explore/current-data))
        last
        )
    (while (and indents (<= curr-point (car indents)))
      (setq last (pop indents))
      )
    (if (car indents)
        (progn
          (move-to-column (car indents))
          (while (and (not (looking-at "[[:alnum:]-]"))
                      (> (point) start-pos))
            (forward-line -1)
            (move-to-column (car indents))
            )
          )
      (move-to-column 0))
    )
  )
(defun explore/layer-increase ()
  (interactive)
  (save-excursion
    (outline-previous-heading)
    (setq explore/current-data (get-text-property (point) :tree-data))
    )
  (let ((indents (reverse (explore/tree-data-indents explore/current-data)))
        (curr-point (current-column))
        (start-pos (marker-position (explore/tree-data-start-pos explore/current-data)))
        last
        )
    (while (and indents (>= curr-point (car indents)))
      (setq last (pop indents))
      )
    (move-to-column (or (car indents) last))
    (progn
      (while (and (not (looking-at "[[:alnum:]-]"))
                  (> (point) start-pos))
        (forward-line -1)
        (move-to-column (or (car indents) last))
        )
      )
    )
  )
;;--------------------------------------------------
;; Setup

(defun explore/initial-setup (&optional tree)
  """ An initial setup for a tree """
  (cond
   ((not tree) (progn (setq tree (make-explore/node :name "__root"))
                      (explore/generate-tree tree 5 5)))
   ((equal tree t) (setq tree (make-explore/node :name "__root")))
   )

  (let* ((tree-data (make-explore/tree-data
                     :name (read-string "Tree Name: ")
                     :root tree))
         indent-amount
         )
    (setq explore/current-data tree-data)
    (goto-char (point-max))
    ;; Put the basic data into the heading
    (insert (propertize (format "\n\n** %s: " (explore/tree-data-name explore/current-data))
                        :tree-data explore/current-data))
    ;;store the indent
    (setq indent-amount (current-column))
    (set-marker (explore/tree-data-path-pos explore/current-data) (point))
    (newline)
    ;; draw a divider line
    (move-to-column indent-amount t)
    (insert (make-string (- 80 indent-amount) ?-))
    (newline)
    ;;draw the root node
    (move-to-column indent-amount t)
    (insert (propertize "Root: " :layer 0))
    (push (current-column) (explore/tree-data-indents explore/current-data))
    (set-marker (explore/tree-data-start-pos explore/current-data) (point))
    ;;Draw all children, indented, with " : " after
    (explore/draw-children)
    (goto-char (point-max))
    (newline 2)
    )
  )

(defvar-local explore-mode-map
  (let ((map (make-keymap)))
    ;; (define-key map "RET" 'explore/expand-entry)
    ;;any potential functions? add particular structures?
    ;;TODO: M-RET to prev-line
    map)
  "Basic Keymap for Explore Mode"
  )
(add-to-list 'auto-mode-alist '("\\.seq\\'" . sequence-mode))

(defconst explore-font-lock-keywords
  (list
   `("^\\(**\\) \\([[:alnum:]]+\\)"
     (0 "font-lock-keyword-face")
     (1 "font-lock-constant-face"))
   )
  "Highlighting of Explore entry titles"
  )

(defvar explore-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?- "." st)
    st)
  "Syntax table for Explore-mode"
  )

(define-derived-mode explore-mode fundamental-mode "Explore Mode"
  "Major Mode for exploring Trees"
  (interactive)
  (kill-all-local-variables)
  (use-local-map explore-mode-map)
  (set (make-local-variable 'font-lock-defaults) (list explore-font-lock-keywords nil))
  (set-syntax-table explore-mode-syntax-table)
  (setq major-mode 'explore-mode
        mode-name "EXPLORE")
  (set-marker explore/next-heading (point-max))
  (outline-minor-mode)
  (run-mode-hooks)
  )

(provide 'explore-mode)
