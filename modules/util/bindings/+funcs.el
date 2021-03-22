;;; util/bindings/+funcs.el -*- lexical-binding: t; -*-

(defun +jg-bindings-goto-org-agenda-file ()
  (interactive)
  (let ((agenda (car org-agenda-files)))
    (find-file agenda)
    )
  )
(defun +jg-bindings-goto-messages ()
  (interactive)
  (switch-to-buffer "*Messages*")
  )
(defun +jg-bindings-goto-home ()
  (interactive)
  (find-file "~")
  )
(defun +jg-bindings-goto-resources ()
  (interactive)
  (find-file "~/github/writing/resources")
  )
(defun +jg-bindings-goto-desktop ()
  (interactive)
  (find-file "~/Desktop")
  )
(defun +jg-bindings-goto-github ()
  (interactive)
  (find-file "~/github")
  )
(defun +jg-bindings-goto-mega ()
  (interactive)
  (find-file "~/mega")
  )
(defun +jg-bindings-open-link ()
  (interactive)
  (cond ((eq evil-state 'visual)
         (let ((str (buffer-substring-no-properties evil-visual-beginning evil-visual-end)))
           (org-open-link-from-string (format "[[%s]]" (string-trim str)))
           ))
        (t (org-open-at-point 'in-emacs))
        )
  )
(defun +jg-bindings-open-link-externally ()
  (interactive)
  (let ((current-prefix-arg '(16))
        (str (if (eq evil-state 'visual) (buffer-substring-no-properties evil-visual-beginning evil-visual-end) nil))
        )
    (cond ((eq evil-state 'visual)
           (funcall-interactively 'org-open-link-from-string (format "[[%s]]" (string-trim str))))
          ((eq 'link (org-element-type (org-element-context)))
           (call-interactively 'org-open-at-point))
          (t
           (funcall-interactively 'org-open-link-from-string
                                  (format "[[%s]]" (string-trim (buffer-substring-no-properties (line-beginning-position)
                                                                                                (line-end-position))))))
          )
    )
  )

(defun +jg-bindings-goto-random-line ()
  (interactive)
  (let ((max-line (line-number-at-pos (point-max))))
    (goto-char (point-min))
    (forward-line (random max-line))
    (evil-beginning-of-line)
    )
  )

(defun +jg-bindings-clear-buffer ()
  """ Utility to clear a buffer
    from https://stackoverflow.com/questions/24565068/ """
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer))
  )

(defun +jg-personal-insert-lparen ()
  """ utility to insert a (  """
  (interactive)
  (insert "(")
  )
(defun +jg-personal-insert-rparen ()
  """ utility to insert a ) """
  (interactive)
  (insert ")")
  )


(defun +jg-ibuffer-filter-setup ()
  (interactive)
  (ibuffer-clear-filter-groups)
  (ibuffer-filter-disable)

  ;; Use +jg-ibuffer-defaults
  (ibuffer-projectile-set-filter-groups)
  (ibuffer-add-saved-filters "default")
  )

;;;###autoload
(defun jg-toggle-narrow-buffer (arg)
  "Narrow the buffer to BEG END. If narrowed, widen it.
If region isn't active, narrow away anything above point
"
  (interactive "P")
  (cond ((eq evil-state 'normal)
         (narrow-to-region (line-beginning-position) (point-max)))
        ((eq evil-state 'visual)
         (narrow-to-region evil-visual-beginning evil-visual-end))
        )
  )
;; From spacemacs-defaults
;; from @bmag
(defun spacemacs/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun spacemacs/rotate-windows-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))

(defun spacemacs/rotate-windows-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (spacemacs/rotate-windows-forward (* -1 count)))

(defun jg-narrowing-move-focus-backward (arg)
  (interactive "p")
  (jg-narrowing-move-focus-forward(- arg))
  )
(defun jg-narrowing-move-focus-forward (arg)
  (interactive "p")
  (widen)
  (evil-forward-section-begin arg)
  (let ((bounds (+evil:defun-txtobj)))
    (narrow-to-region (car bounds) (cadr bounds))
    )
  )

(when (featurep! :ui workspaces)
  (defun jg-counsel-workspace ()
    "Forward to `' or `workspace-set' if workspace doesn't exist."
    (interactive)
    (require 'bookmark)
    (ivy-read "Create or jump to workspace: "
              (+workspace-list-names)
              :history 'workspace-history
              :action (lambda (x)
                        (message "Got: %s" x)
                        (cond ((string-equal x (+workspace-current-name))
                               (message "Eq")
                               (+workspace-save x))
                              (t
                               (+workspace-switch x t))))
              :caller 'counsel-workspace)
    )
  )

(defun +jg-toggle-line-numbers ()
  (interactive)
  (setq display-line-numbers (if (not (eq display-line-numbers t)) t nil))
  )

(defun +jg-toggle-line-numbers-visual ()
  (interactive)
  (setq display-line-numbers (if (not (eq display-line-numbers 'visual)) 'visual nil))
  )

(defun jg-browse-url()
  (interactive)
  (let ((url (cond ((eq evil-state 'visual)
                    (buffer-substring-no-properties evil-visual-beginning evil-visual-end))
                   (t
                    (read-string "Search For: "))
                   ))
        )
    (cond ((f-exists? url)
           (shell-command (format "open %s" url)))
          ((s-prefix? "http" url)
           (browse-url url))
          ((s-prefix? "www." url)
           (browse-url (format "https://%s" url)))
          ((string-match ".com\\|.uk" url)
           (browse-url (format "https://%s" url)))
          (t
           (browse-url (format jg-google-url url)))
          )
    )
  )
(defun jg-browse-twitter ()
  (interactive)
  (browse-url jg-twitter-url)
  )
