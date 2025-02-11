;;; +related.el -*- lexical-binding: t; -*-
(require 'macro-tools--related)

;;;###autoload
(defun +jg-projects-find-related ()
  " Get related files or directories, if it exists, open it "
  (interactive)
  (-when-let* ((buff (if (eq major-mode 'dired-mode) (+jg-projects-find-related-directory) (projectile--find-related-file (buffer-file-name))))
               (buff-exists (f-exists? buff))
               (wind-fn (cond ((fboundp 'jg-workspaces-find-buff-fn)
                               jg-workspaces-find-buff-fn)
                              ((eq major-mode 'dired-mode)
                               #'find-file)
                              (t
                               #'+jg-workspace-default-new-window)))
               )
    (funcall wind-fn buff)
    )
  )

(defun +jg-projects-find-related-directory () ;; -> buffer
  (message "Finding Related Directories")
  (let* ((current default-directory)
         (parent (f-parent current))
         (root (projectile-project-root))
         (proj-type (projectile-project-type))
         (proj (alist-get proj-type projectile-project-types))
         (related-file (f-join root jg-projects-related-dir-file))
         (read-related (+jg-workspace-read-related-file related-file root))
         (available (when read-related read-related))
         key
        )
    ;; populate available
    (push `(:root . ,root) available)
    (unless read-related
        (when (f-exists? (f-join current "__tests"))            (push `(:local-tests . ,(f-join current "__tests")) available))
        (when (f-exists? (f-join root "docs"))                  (push `(:docs        . ,(f-join root "docs")) available))
        (when (f-exists? (f-join root ".tasks"))                (push `(:tasks       . ,(f-join root ".tasks")) available))
        (when (f-exists? (f-join root ".wiki"))                 (push `(:wiki        . ,(f-join root ".wiki")) available))
        (when (f-exists? (f-join root ".github/workflows"))     (push `(:github-workflows . ,(f-join root ".github/workflows")) available))

        (when (f-exists? (f-join parent "_structs"))          (push `(:structs     . ,(f-join parent "_structs")) available))
        (when (f-exists? (f-join parent "_interfaces"))       (push `(:interfaces  . ,(f-join parent "_interfaces")) available))
        (when (f-exists? (f-join parent"_abstract"))          (push `(:abstract    . ,(f-join parent "_abstract")) available))

        (when (f-exists? (f-join parent "__data"))             (push `(:data        . ,(f-join parent "__data")) available))
        (when (f-exists? (f-join parent "__templates"))        (push `(:templates   . ,(f-join parent "__templates")) available))
        (push `(:default . root) available)
        )

    ;; select one
    (setq key (ivy-read "Available Directories: " available :require-match t))
    (alist-get (intern key) available nil nil #'equal)
    )
  )

(defun +jg-workspace-default-new-window (buff)
  (select-window (split-window-right))
  (find-file buff)
  )

(defun +jg-workspace-read-related-file (file root) ;; -> alist
  "Read a file of #comments, and name : path pairs
 for quick navigation of project dirs
"
  (when (f-exists? file)
    (let (result)
      (with-temp-buffer (insert-file-contents file)
                        (goto-char (point-min))
                        (flush-lines "^;;")
                        (while (not (eobp))
                          (-when-let* ((vals (split-string (buffer-substring (line-beginning-position) (line-end-position)) " : " t "\s+") result)
                                       (exists (f-exists? (expand-file-name (cadr vals) root)))
                                       )
                            (push (cons (intern (car vals)) (expand-file-name (cadr vals) root)) result)
                            )
                          (forward-line)
                          )
                        )
      result
      )
    )
  )
