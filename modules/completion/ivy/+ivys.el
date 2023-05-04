;;; +ivys.el -*- lexical-binding: t; -*-

;;-- utils

(defun +jg-completion-ivy-kill-buffer (buff)
  (interactive)
  (with-current-buffer buff
    (kill-current-buffer)
    )
  )
;;-- end utils

;;-- actions
;; Overrides find-file's actions to add "f" for find literally
(ivy-add-actions 'counsel-find-file
                 '(("f" (lambda (x) (find-file-literally x)) "Fundamental")))

(ivy-add-actions 'ivy-switch-buffer
                 '(
                   ("k" +jg-completion-ivy-kill-buffer "Kill")
                   )
                 )

(defun +jg-completion-workspace-switch (x)
  (cond ((string-equal x (+workspace-current-name))
         (+workspace-save x))
        ((+workspace-exists-p x)
         (+workspace-switch x))
        ((-contains? (bookmark-all-names) x)
         (+workspace-switch x t)
         (bookmark-jump x))
        (t (+workspace-switch x t))
        )
  )
;;-- end actions

;;-- ivys

(defun +jg-completion-ivy-bookmark ()
  "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist.
Modified to pre-sort bookmarks, caselessly
"
  (interactive)
  (require 'bookmark)
  (ivy-read "Create or jump to bookmark: "
            (cl-sort (bookmark-all-names) #'string-lessp :key #'downcase)
            :history 'bookmark-history
            :action (lambda (x)
                      (cond ((and counsel-bookmark-avoid-dired
                                  (member x (bookmark-all-names))
                                  (file-directory-p (bookmark-location x)))
                             (with-ivy-window
                               (let (
(default-directory (bookmark-location x)))
                                 (counsel-find-file))))
                            ((member x (bookmark-all-names))
                             (with-ivy-window
                               (bookmark-jump x)))
                            (t
                             (bookmark-set x))))
            :caller 'counsel-bookmark))

(defun +jg-completion-ivy-features ()
  " Insert from a list of recognized features "
  (interactive)
  (ivy-read "Available Features: "
            (seq-sort #'string-lessp features)
            :require-match nil
            :action 'insert
            )
  )

;;-- end ivys
