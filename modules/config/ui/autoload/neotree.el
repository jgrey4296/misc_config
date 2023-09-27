;;; neotree.el -*- lexical-binding: t; -*-

;; `neotree-show' and `neotree-find' don't respect the current project, and open
;; neotree in `default-directory'. `+neotree/open' and `neotree/find-this-file'
;; will ensure the neotree pane is always rooted in the project root.

;;;###autoload
(defun +neotree/collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (if (and (file-directory-p node)
             (neo-buffer--expanded-node-p node))
        (+neotree/collapse)
      (neotree-select-up-node))))

;;;###autoload
(defun +neotree/collapse ()
  "Collapse a neotree node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (when (file-directory-p node)
      (neo-buffer--set-expand node nil)
      (neo-buffer--refresh t))
    (when neo-auto-indent-point
      (neo-point-auto-indent))))

;;;###autoload
(defun +neotree/expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (cond ((file-directory-p node)
           (neo-buffer--set-expand node t)
           (neo-buffer--refresh t)
           (when neo-auto-indent-point
             (forward-line)
             (neo-point-auto-indent)))
          (t
           (call-interactively #'neotree-enter)))))

;;;###autoload
(defun +neotree-fix-cursor-h (&rest _)
  ;; The cursor always sits at bol. `+neotree--fix-cursor-h' and
  ;; `+neotree--indent-cursor-a' change that behavior so that the cursor is
  ;; always on the first non-blank character on the line, in the neo buffer.
  (with-current-buffer neo-global--buffer
    (+neotree--indent-cursor-a)))

;;;###autoload
(defun +neotree--indent-cursor-a (&rest _)
  (beginning-of-line)
  (skip-chars-forward " \t\r")
  )

;;;###autoload
(advice-add 'neotree-next-line :after #'+neotree--indent-cursor-a)

;;;###autoload
(advice-add 'neotree-previous-line :after #'+neotree--indent-cursor-a)
