(require 'dash)

(cl-defstruct trie-tree/node name value
              (children (make-hash-table :test 'equal)))

;; Tree Operations
(defun trie-tree/generate-tree (node n-children layers)
  """ Create a tree of n-children and layers, using random dictionary words as nodes """
  (if (> layers 0)
      (with-temp-buffer
        (insert-file-contents "/usr/share/dict/words")
        (let* ((words (mapcar (lambda (x)
                                (goto-char (random (+ 1 (point-max))))
                                (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                              (make-list (+ 1 (random n-children)) "_")))
               (new-nodes (mapcar (lambda (x) (trie-tree/node-add-child node x))
                                  words))
               )
          (mapcar (lambda (x) (trie-tree/generate-tree x n-children (- layers 1))) new-nodes)
          )
        )
    )
  )
(defun trie-tree/tree-add (rootnode path val)
  """ Add a node with a value as a leaf of path from rootnode, creating
intermediate nodes if necessary """
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (setq curr-node (if (trie-tree/node-has-child curr-node curr-path)
                          (trie-tree/node-get-child curr-node curr-path)
                        (trie-tree/node-add-child curr-node curr-path))
            curr-path (pop path))
      )
    )
  )
(defun trie-tree/tree-children (rootnode path)
  """ Get the children of a node, using a path from the root """
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (if (trie-tree/node-has-child curr-node curr-path)
          (setq curr-node (trie-tree/node-get-child curr-node curr-path)
                curr-path (pop path))
        (setq curr-path nil))
      )
    (if curr-node
        (hash-table-keys (trie-tree/node-children curr-node))
      '())
  )
)
(defun trie-tree/tree-get (rootnode path)
  """ Get a node using a path from the rootnode """
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (if (trie-tree/node-has-child curr-node curr-path)
          (setq curr-node (trie-tree/node-get-child curr-node curr-path)
                curr-path (pop path))
        (setq curr-path nil))
      )
    (if curr-node
        curr-node
      nil)
    )
  )
(defun trie-tree/tree-remove (rootnode path child)
  " Remove the leaf of a path from the tree "
  (message "Removing %s from %s " child path)
  (let ((node (trie-tree/tree-get rootnode path)))
    (if (trie-tree/node-has-child node child)
        (remhash child (trie-tree/node-children node))
        )
    )
  )
(defun trie-tree/dfs-tree (n pred)
  """ Apply Pred to each node, and return the nodes that pass """
  (if (hash-table-empty-p (trie-tree/node-children n))
      ;;base case
      (if (apply pred n)
          n
        nil
        )
      ;;recursive case
    (-flatten
     (cons (if (apply pred n) n nil)
           (mapcar 'trie-tree/dfs-tree
                   (hash-table-values (trie-tree/node-children n)))))
    )
)

;;Node Operations
(defun trie-tree/node-to-string (n)
  """ Convert a Tree Node to a User Understandable String """
  (format "Trie-Tree/Node: %s %s (%s)" (trie-tree/node-name n)
          (trie-tree/node-value n)
          (string-join (hash-table-keys (trie-tree/node-children n)) ", "))
  )
(defun trie-tree/node-add-child (n childname)
  """ Add a new node of childname to node n """
  (puthash childname (make-trie-tree/node :name childname)
           (trie-tree/node-children n)))
(defun trie-tree/node-has-child (n childname)
  """ Check if a node has a child by the name of childname """
  (-contains? (hash-table-keys (trie-tree/node-children n)) childname))
(defun trie-tree/node-get-child (n childname)
  """ Get the child of a node """
  (gethash childname (trie-tree/node-children n)))

(provide 'trie-tree)
;;Example:
;; (setq root (make-trie-tree/node :name "__root"))
;; (trie-tree/tree-add root '("a" "b" "c") "value")
;; (trie-tree/tree-add root '("a" "b" "d") "value")
;; (message "%s" (trie-tree/node-to-string root))
;; (trie-tree/tree-children root '("periostea" "disquisitional" "unimplicitly" "Champlain"))
;; (trie-tree/generate-tree root 3 4)
;; (with-temp-buffer
;;   (insert-file-contents "/usr/share/dict/words")
;;   (mapcar (lambda (x)
;;             (goto-char (random (point-max)))
;;             (buffer-substring (line-beginning-position)
;;                               (line-end-position)))
;;           (make-list 5 "_")))
