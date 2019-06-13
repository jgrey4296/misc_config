(require 'dash)
(require 'tree-gen)

(defvar-local explore/overlays (make-hash-table))
(defvar-local explore/overlay-max 20)
(defvar-local explore/inspector-overlay nil)
(defvar-local explore/tree-root (make-explore/node :name "__root"))
(defvar-local explore/indents '())
(defvar-local explore/current-path '())
(defvar-local explore/start-pos 0)
(defvar-local explore/max-lines 0)

;;--------------------------------------------------
;; Overlays

(defun explore/make-overlay (beg end layer)
  (let ((overlay (if (gethash layer explore/overlays)
                     (gethash layer explore/overlays)
                   (puthash layer (make-overlay 1 2) explore/overlays)))
        (color "green"))
    (overlay-put overlay 'face `((foreground-color . ,color)))
    (move-overlay overlay beg (- end 2))
    )
  )

(defun explore/clear-overlays ()
  (loop for x in (hash-table-values explore/overlays) do
        (delete-overlay x)
        )
  (clrhash explore/overlays)
  )

;;--------------------------------------------------
;; Utils

(defmacro explore/pop-list-to-length (lst n)
  `(progn (while (> (length ,lst) ,n)
            (pop ,lst)
            )
          (car ,lst))
  )

(defun explore/pos-to-bounds (seq col)
  ;; (message "Pos to Bounds Col: %s Seq: %s" col seq)
  (let* ((indents (copy-seq seq))
         (upper 0)
         (lower 0)
         )
    (while (and indents (< col (car indents)))
      (setq upper (pop indents)
            lower (car indents))

      )
    `(,(+ upper (if (equal lower upper) 1 0)) ,lower)
    )
  )

(defun explore/cols-to-pos (bounds)
  (let* ((lower (cadr bounds))
         (upper (car bounds))
         )
         `(,(explore/col-to-pos upper) ,(explore/col-to-pos lower))
    )
  )

(defun explore/col-to-pos (col)
  (save-excursion
    (move-to-column col)
    (point))
  )

;;--------------------------------------------------
;; Drawing

(defun explore/draw-children (key layer)
    (let* ((node (explore/tree-get explore/tree-root (reverse (cons key explore/current-path))))
           (children (explore/node-children node))
           (num_children (length (hash-table-values children)))
           (maxlen (apply 'max (mapcar (lambda (x) (length x))
                                       (hash-table-keys children))))
           (indent_lower (explore/pop-list-to-length explore/indents layer))
           (indent_upper (+ indent_lower maxlen 3))
           )
      (message "Drawing Children for: %s" (string-join (reverse (cons key explore/current-path)) "\\"))
      (message "Children: %s" (string-join (hash-table-keys children) ", "))
      ;;move to the indent head position
      (push indent_upper explore/indents)
      (push key explore/current-path)
      (setq explore/max-lines (max explore/max-lines num_children))
      (goto-char explore/start-pos)
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
                  ) (hash-table-keys children))
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

;;--------------------------------------------------
;; Interaction

(defun explore/expand-entry ()
  (interactive)
  (if (> (current-column) (car (last explore/indents)))
      (let* ((bounds (explore/pos-to-bounds explore/indents (current-column)))
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
)

;;--------------------------------------------------
;; Setup

(defun explore/initial-setup (tree)
  (setq explore/indents '()
        explore/current-path '()
        explore/max-lines 0
        explore/overlays (make-hash-table)
        )
  (goto-char (point-min))
  (insert "\n\n\n\n")
  (insert "Test Tree:") ;; / tree name
  ;;store the indent
  (push (current-column) explore/indents)
  (newline)
  ;;draw the root node
  (move-to-column (car explore/indents) 't)
  (insert "Root: ")
  (push (current-column) explore/indents)
  (setq explore/start-pos (point))
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
  (make-variable-buffer-local 'explore/tree-root)
  (explore/generate-tree explore/tree-root 5 5)
)


(provide 'explore-mode)
