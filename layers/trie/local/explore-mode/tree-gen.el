(require 'dash)
(provide 'tree-gen)

(defstruct explore/node name value (children (make-hash-table :test 'equal)))

(defun explore/dfs-tree (n pred)
  """ Apply Pred to each node, and return the nodes that pass """
  (if (hash-table-empty-p (explore/node-children n))
      ;;base case
      (if (apply pred n)
          n
        nil
        )
      ;;recursive case
    (-flatten
     (cons (if (apply pred n) n nil)
           (mapcar 'explore/dfs-tree
                   (hash-table-values (explore/node-children n)))))
    )
)

(defun explore/node-to-string (n)
  """ Convert a Tree Node to a User Understandable String """
  (format "Explore/Node: %s %s (%s)" (explore/node-name n)
          (explore/node-value n)
          (string-join (hash-table-keys (explore/node-children n)) ", "))
  )

(defun explore/node-add-child (n childname)
  """ Add a new node of childname to node n """
  (puthash childname (make-explore/node :name childname)
           (explore/node-children n)))

(defun explore/node-has-child (n childname)
  """ Check if a node has a child by the name of childname """
  (-contains? (hash-table-keys (explore/node-children n)) childname))

(defun explore/node-get-child (n childname)
  """ Get the child of a node """
  (gethash childname (explore/node-children n)))

(defun explore/tree-add (rootnode path val)
  """ Add a node with a value as a leaf of path from rootnode, creating
intermediate nodes if necessary """
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (setq curr-node (if (explore/node-has-child curr-node curr-path)
                          (explore/node-get-child curr-node curr-path)
                        (explore/node-add-child curr-node curr-path))
            curr-path (pop path))
      )
    )
  )

(defun explore/tree-children (rootnode path)
  """ Get the children of a node, using a path from the root """
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (if (explore/node-has-child curr-node curr-path)
          (setq curr-node (explore/node-get-child curr-node curr-path)
                curr-path (pop path))
        (setq curr-path nil))
      )
    (if curr-node
        (hash-table-keys (explore/node-children curr-node))
      '())
  )
)

(defun explore/tree-get (rootnode path)
  """ Get a node using a path from the rootnode """
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (if (explore/node-has-child curr-node curr-path)
          (setq curr-node (explore/node-get-child curr-node curr-path)
                curr-path (pop path))
        (setq curr-path nil))
      )
    (if curr-node
        curr-node
      nil)
    )
  )

(defun explore/generate-tree (node n-children layers)
  """ Create a tree of n-children and layers, using random dictionary words as nodes """
  (if (> layers 0)
      (with-temp-buffer
        (insert-file-contents "/usr/share/dict/words")
        (let* ((words (mapcar (lambda (x)
                                (goto-char (random (+ 1 (point-max))))
                                (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                              (make-list (+ 1 (random n-children)) "_")))
               (new-nodes (mapcar (lambda (x) (explore/node-add-child node x))
                                  words))
               )
          (mapcar (lambda (x) (explore/generate-tree x n-children (- layers 1))) new-nodes)
          )
        )
    )
  )

;; (setq root (make-explore/node :name "__root"))
;; (explore/tree-add root '("a" "b" "c") "value")
;; (explore/tree-add root '("a" "b" "d") "value")
;; (message "%s" (explore/node-to-string root))
;; (explore/tree-children root '("periostea" "disquisitional" "unimplicitly" "Champlain"))
;; (explore/generate-tree root 3 4)
;; (with-temp-buffer
;;   (insert-file-contents "/usr/share/dict/words")
;;   (mapcar (lambda (x)
;;             (goto-char (random (point-max)))
;;             (buffer-substring (line-beginning-position)
;;                               (line-end-position)))
;;           (make-list 5 "_")))
