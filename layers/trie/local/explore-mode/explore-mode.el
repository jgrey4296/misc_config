(require 'dash)
(require 'tree-gen)

(defstruct explore/tree-data name root (indents '()) (curr-path '()) (max-lines 0) (start-pos (make-marker)) (path-pos (make-marker)) (overlays (make-hash-table)))

(defvar-local explore/current-markers (list (make-marker) (make-marker)))
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
  """ Given a key and the layer it inhabits, draw the children of the node, safely
(ie: do nothing if there are no children, but do update the path) """
  (let* ((rev-path (reverse (explore/tree-data-curr-path explore/current-data)))
         (node (explore/tree-get (explore/tree-data-root explore/current-data) rev-path))
         (children (explore/node-children node))
         (num_children (length (hash-table-values children)))
         (layer (+ 1 (length rev-path)))
         )
    (progn
      ;; (message "Drawing Children for: %s" (string-join rev-path "\\"))
      ;; (message "Children: %s" (string-join (hash-table-keys children) ", "))
      ;;Draw
      (explore/draw-children-safe node layer))
    ;;todo: else clause that clears lines anyway
    )
  )

(defun explore/draw-children-safe (node layer)
  """ Given a node and a layer, draw the children of the node, without checking
    if there are children to draw """
  (let* ((children (explore/node-children node))
         (num_children (length (hash-table-values children)))
         (maxlen (if (hash-table-empty-p children) 20
                   (apply 'max (mapcar (lambda (x) (length x))
                                       (hash-table-keys children)))))
         (indent_lower (car (explore/tree-data-indents explore/current-data)))
         (indent_upper (+ indent_lower maxlen))
         )
    ;;move to the indent head position
    (goto-char (marker-position (explore/tree-data-start-pos explore/current-data)))
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
                (if (> (forward-line 1) 0) (newline))
                ) (sort (hash-table-keys children) (lambda (x y) (string-lessp (downcase x) (downcase y)))))
    ;; Clear any remaining lines
    (if (> (explore/tree-data-max-lines explore/current-data) num_children)
        (seq-each (lambda (x)
                    (move-to-column indent_lower t)
                    (delete-region (point) (line-end-position))
                    (forward-line 1)
                    )
                  (make-list (- (explore/tree-data-max-lines explore/current-data) num_children) nil)
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
    (add-face-text-property (marker-position (explore/tree-data-path-pos explore/current-data)) (line-end-position)
                            '(:foreground "red"))
    )
  )

;;--------------------------------------------------
;; Interaction

(defun explore/update-tree-data()
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
  ;; (goto-char start-pos)
  ;; (move-to-column (if (eq layer 0) (car indents) (cadr indents)))
  )

(defun explore/insert-entry ()
  """ insert a new node into the tree """
  (interactive)
  (explore/update-tree-data)

  (let* ((curr-data explore/current-data)
         (parent-path (explore/tree-data-curr-path curr-data))
         (node (explore/tree-get (explore/tree-data-root curr-data) (reverse parent-path)))
         (bounds (explore/cols-to-pos (explore/cols-to-bounds (explore/tree-data-indents curr-data) (current-column))))
         (substr (string-trim (buffer-substring-no-properties (cadr bounds) (- (car bounds) 2))))
         )
    (if (not (eq explore/current-layer (length parent-path)))
        (message "Path / Layer Mismatch")
      (progn
        ;; (message "PathLen : %s" (length parent-path))
        ;; (message "Path : %s" (string-join parent-path "\\"))
        ;; (message "Bounds: %s - %s" (car bounds) (cadr bounds))
        ;; (message "New Node: %s" substr)
        (explore/node-add-child node substr)
        (explore/expand-entry)
        )
      )
    )
  )

(defun explore/decrease-layer ()
  (interactive)
  ;;current position -> layer -*> new layer -> update tree-data -> redraw
  (explore/update-tree-data)
  ;;move

  (explore/update-tree-data)
  )

(defun explore/increase-layer ()
  (interactive)
  (explore/update-tree-data)
  ;;move

  (explore/update-tree-data)
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
;;(add-to-list 'auto-mode-alist '("\\.seq\\'" . sequence-mode))

(define-derived-mode explore-mode fundamental-mode "Explore Mode"
  "Major Mode for exploring Trees"
  (interactive)
  (kill-all-local-variables)
  (use-local-map explore-mode-map)
  (setq major-mode 'explore-mode
        mode-name "EXPLORE")
  (outline-minor-mode)
  )

(provide 'explore-mode)
