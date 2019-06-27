(require 'dash)
(require 'tree-gen)

(defvar-local explore/overlays (make-hash-table))
(defvar-local explore/overlay-max 20)
(defvar-local explore/inspector-overlay nil)
(defvar-local explore/tree-root nil)
(defvar-local explore/indents '())
(defvar-local explore/current-path '())
(defvar-local explore/start-pos (make-marker))
(defvar-local explore/max-lines 0)
(defvar-local explore/path-pos (make-marker))

;;--------------------------------------------------
;; Overlays

(defun explore/make-overlay (beg end layer)
  """ Create or reuse an overlay to use for a particular layer in the tree """
  (let ((overlay (if (gethash layer explore/overlays)
                     (gethash layer explore/overlays)
                   (puthash layer (make-overlay 1 2) explore/overlays)))
        (color "green"))
    (overlay-put overlay 'face `((foreground-color . ,color)))
    (move-overlay overlay beg (- end 2))
    )
  )

(defun explore/clear-overlays ()
  """ Clear all overlays """
  (loop for x in (hash-table-values explore/overlays) do
        (delete-overlay x)
        )
  (clrhash explore/overlays)
  )

;;--------------------------------------------------
;; Utils

(defmacro explore/pop-list-to-length (lst n)
  """ Pop values off a list IN PLACE until it is a given length,
then return its new head """
  `(progn (while (> (length ,lst) ,n)
            (pop ,lst)
            )
          (car ,lst))
  )

(defmacro explore/reduce-list-length (lst n)
  """ Pop values off a DUPLICATED list until it is a given length,
then return the modified list """
  `(let* ((the_list (seq-copy ,lst)))
     (while (> (length the_list) ,n)
       (pop the_list)
       )
     the_list)
  )

(defun explore/col-to-bounds (seq col)
  """ Given a sequence of indents and a column value,
calculate the bounds that column falls within """
  (let* ((indents (copy-seq seq))
         (upper 0)
         (lower 0)
         )
    (if (> col (car indents))
        (setq lower (car indents)
              upper (+ lower 20))
      (while (and indents (< col (car indents)))
        (setq upper (pop indents)
              lower (car indents))
        )
      )
    (message "Upper: %s Lower: %s" upper lower)
    `(,(+ upper (if (equal lower upper) 1 0)) ,lower)
    )
  )

(defun explore/cols-to-pos (bounds)
  """ Convert a pair of bounding columns to actual buffer positions """
  (let* ((lower (cadr bounds))
         (upper (car bounds))
         )
         `(,(explore/col-to-pos upper) ,(explore/col-to-pos lower))
    )
  )

(defun explore/col-to-pos (col)
  """ Convert a column value to an actual buffer position """
  (save-excursion
    (move-to-column col 't)
    (point))
  )

(defun explore/col-to-layer (col)
  """ Convert a column value to the corresponding layer of the tree """
  (let* ((indents (cddr (reverse (copy-seq explore/indents))))
         (cal-layer 0)
         )
    (while (and indents (> col (car indents)))
      (pop indents)
      (incf cal-layer)
    )
    (message "Col to Layer: %s -> %s" col cal-layer)
    cal-layer
  )
)
;;--------------------------------------------------
;; Drawing

(defun explore/draw-children (key layer)
  """ Given a key and the layer it inhabits, draw the children of the node, safely
(ie: do nothing if there are no children, but do update the path) """
  (let* ((node (explore/tree-get explore/tree-root (reverse (cons key explore/current-path))))
         (children (explore/node-children node))
         (num_children (length (hash-table-values children)))
         )
    (if (> num_children 0)
        (progn
          (message "Drawing Children for: %s" (string-join (reverse (cons key explore/current-path)) "\\"))
          (message "Children: %s" (string-join (hash-table-keys children) ", "))
          (explore/draw-children-safe node layer))
      )
    )
    (push key explore/current-path)
)

(defun explore/draw-children-safe (node layer)
  """ Given a node and a layer, draw the children of the node, without checking
    if there are children to draw """
    (let* ((children (explore/node-children node))
           (num_children (length (hash-table-values children)))
           (maxlen (apply 'max (mapcar (lambda (x) (length x))
                                       (hash-table-keys children))))
           (indent_lower (explore/pop-list-to-length explore/indents layer))
           (indent_upper (+ indent_lower maxlen 3))
           )
     ;;move to the indent head position
      (push indent_upper explore/indents)
      (setq explore/max-lines (max explore/max-lines num_children))
      (goto-char (marker-position explore/start-pos))
      (seq-each (lambda (x)
                  (move-to-column indent_lower 't)
                  (delete-region (point) (line-end-position))
                  ;;TODO: possibly limit size of x
                  (insert x)
                  (put-text-property (explore/col-to-pos indent_lower)
                                     (explore/col-to-pos indent_upper)
                                     :layer layer)
                  (move-to-column (+ indent_lower maxlen) 't)
                  (insert " : ")
                  (if (> (forward-line) 0) (newline))
                  ) (sort (hash-table-keys children) (lambda (x y) (string-lessp (downcase x) (downcase y)))))
      ;; Clear any remaining lines
      (seq-each (lambda (x)
                  (move-to-column indent_lower 't)
                  (delete-region (point) (line-end-position))
                  (forward-line)
                  )
                (make-list (- explore/max-lines num_children) nil)
    )
  )
    nil
)

(defun explore/draw-path ()
  """ Draw the path through the tree currently being used """
  (save-excursion
    (goto-char (marker-position explore/path-pos))
    (delete-region (point) (line-end-position))
    (seq-each (lambda (pair)
                (move-to-column (car pair) 't)
                (insert (cdr pair)))
              (-zip (cdr (reverse explore/indents)) (reverse explore/current-path)))
    (add-face-text-property (marker-position explore/path-pos) (line-end-position)
                            '(:foreground "red"))
    )
  )

;;--------------------------------------------------
;; Interaction

(defun explore/expand-entry ()
  """ Expand the node the user selects """
  (interactive)
  (if (> (current-column) (car (last explore/indents)))
      (let* ((bounds (explore/col-to-bounds explore/indents (current-column)))
             (positions (explore/cols-to-pos bounds))
             (substr (string-trim (buffer-substring (cadr positions) (- (car positions) 3))))
             (layer (get-text-property (point) :layer))
             )
        (explore/pop-list-to-length explore/current-path (- layer 2))
        (message "Expanding Entry: (%s %s) (Layer: %s) %s"
                 (car bounds) (cadr bounds) layer (string-join (cons substr explore/current-path) ":")
                 )
        (message "Max Lines So Far: %s" explore/max-lines)
        ;; retrieve or create an overlay
        (explore/make-overlay (cadr positions) (car positions) layer)
        (save-excursion
          (explore/draw-children substr (+ 1 layer)))
        )
    )
  (goto-char (marker-position explore/start-pos))
  (move-to-column (cadr explore/indents))
  (explore/draw-path)
)

(defun explore/insert-entry ()
  """ Insert a new node into the tree """
  (interactive)
  ;; Get the inserted text
  ;;add it in the path's node
  (message "Inserting a value")
  (let* ((layer (explore/col-to-layer (current-column)))
        (parent-path (-non-nil (explore/reduce-list-length explore/current-path layer)))
        (node (explore/tree-get explore/tree-root (reverse parent-path)))
        (bounds (explore/cols-to-pos (explore/col-to-bounds explore/indents (current-column))))
        (substr (string-trim (buffer-substring-no-properties (cadr bounds) (- (car bounds) 2))))
        )
    (if (not (eq layer (length parent-path)))
        (message "Path / Layer Mismatch")
      (progn
        (message "PathLen : %s" (length parent-path))
        (message "Path : %s" (string-join parent-path "\\"))
        (message "Bounds: %s - %s" (car bounds) (cadr bounds))
        (message "New Node: %s" substr)
        (explore/node-add-child node substr)
        )
      )
  )
)

;;--------------------------------------------------
;; Setup

(defun explore/initial-setup (tree)
  """ An initial setup for a tree """
  (if (not tree)
      (progn (setq explore/tree-root (make-explore/node :name "__root"))
             (explore/generate-tree explore/tree-root 5 5))
    (setq explore/tree-root tree)
      )
  (setq explore/indents '()
        explore/current-path '()
        explore/max-lines 0
        explore/overlays (make-hash-table)
        )

  (goto-char (point-min))
  (insert "\n\n\n\n")
  (insert "Test Tree:") ;; / tree name
  (set-marker explore/path-pos (point))
  ;;store the indent
  (push (current-column) explore/indents)
  (newline)
  ;; draw a divider line
  (insert (string-join (make-list 80 "-") ""))
  (newline)
  ;;draw the root node
  (move-to-column (car explore/indents) 't)
  (insert "Root: ")
  (push (current-column) explore/indents)
  (set-marker explore/start-pos (point))
  ;;Draw all children, indented, with " : " after
  (save-excursion
    (explore/draw-children '() 2))
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
)

(provide 'explore-mode)
