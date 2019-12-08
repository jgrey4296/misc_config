(require 'dash)
(provide 'tree-gen)

(defstruct trie-explore/node name value
           (children (make-hash-table :test 'equal)))

(defun trie-explore/dfs-tree (n pred)
  """ Apply Pred to each node, and return the nodes that pass """
  (if (hash-table-empty-p (trie-explore/node-children n))
      ;;base case
      (if (apply pred n)
          n
        nil
        )
      ;;recursive case
    (-flatten
     (cons (if (apply pred n) n nil)
           (mapcar 'trie-explore/dfs-tree
                   (hash-table-values (trie-explore/node-children n)))))
    )
)

(defun trie-explore/node-to-string (n)
  """ Convert a Tree Node to a User Understandable String """
  (format "Trie-Explore/Node: %s %s (%s)" (trie-explore/node-name n)
          (trie-explore/node-value n)
          (string-join (hash-table-keys (trie-explore/node-children n)) ", "))
  )

(defun trie-explore/node-add-child (n childname)
  """ Add a new node of childname to node n """
  (puthash childname (make-trie-explore/node :name childname)
           (trie-explore/node-children n)))
(defun trie-explore/node-has-child (n childname)
  """ Check if a node has a child by the name of childname """
  (-contains? (hash-table-keys (trie-explore/node-children n)) childname))
(defun trie-explore/node-get-child (n childname)
  """ Get the child of a node """
  (gethash childname (trie-explore/node-children n)))
(defun trie-explore/tree-add (rootnode path val)
  """ Add a node with a value as a leaf of path from rootnode, creating
intermediate nodes if necessary """
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (setq curr-node (if (trie-explore/node-has-child curr-node curr-path)
                          (trie-explore/node-get-child curr-node curr-path)
                        (trie-explore/node-add-child curr-node curr-path))
            curr-path (pop path))
      )
    )
  )
(defun trie-explore/tree-children (rootnode path)
  """ Get the children of a node, using a path from the root """
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (if (trie-explore/node-has-child curr-node curr-path)
          (setq curr-node (trie-explore/node-get-child curr-node curr-path)
                curr-path (pop path))
        (setq curr-path nil))
      )
    (if curr-node
        (hash-table-keys (trie-explore/node-children curr-node))
      '())
  )
)
(defun trie-explore/tree-get (rootnode path)
  """ Get a node using a path from the rootnode """
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (if (trie-explore/node-has-child curr-node curr-path)
          (setq curr-node (trie-explore/node-get-child curr-node curr-path)
                curr-path (pop path))
        (setq curr-path nil))
      )
    (if curr-node
        curr-node
      nil)
    )
  )
(defun trie-explore/tree-remove (rootnode path child)
  " Remove the leaf of a path from the tree "
  (message "Removing %s from %s " child path)
  (let ((node (trie-explore/tree-get rootnode path)))
    (if (trie-explore/node-has-child node child)
        (remhash child (trie-explore/node-children node))
        )
    )
  )
(defun trie-explore/generate-tree (node n-children layers)
  """ Create a tree of n-children and layers, using random dictionary words as nodes """
  (if (> layers 0)
      (with-temp-buffer
        (insert-file-contents "/usr/share/dict/words")
        (let* ((words (mapcar (lambda (x)
                                (goto-char (random (+ 1 (point-max))))
                                (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                              (make-list (+ 1 (random n-children)) "_")))
               (new-nodes (mapcar (lambda (x) (trie-explore/node-add-child node x))
                                  words))
               )
          (mapcar (lambda (x) (trie-explore/generate-tree x n-children (- layers 1))) new-nodes)
          )
        )
    )
  )

;; (setq root (make-trie-explore/node :name "__root"))
;; (trie-explore/tree-add root '("a" "b" "c") "value")
;; (trie-explore/tree-add root '("a" "b" "d") "value")
;; (message "%s" (trie-explore/node-to-string root))
;; (trie-explore/tree-children root '("periostea" "disquisitional" "unimplicitly" "Champlain"))
;; (trie-explore/generate-tree root 3 4)
;; (with-temp-buffer
;;   (insert-file-contents "/usr/share/dict/words")
;;   (mapcar (lambda (x)
;;             (goto-char (random (point-max)))
;;             (buffer-substring (line-beginning-position)
;;                               (line-end-position)))
;;           (make-list 5 "_")))
