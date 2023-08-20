;;; emacs/jg-vc/autoload/hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vc-start-in-insert-state-maybe-h ()
  "Start git-commit-mode in insert state if in a blank commit message,
otherwise in default state."
  (when (and (bound-and-true-p evil-mode)
             (not (evil-emacs-state-p))
             (bobp) (eolp))
    (evil-insert-state)))

;;;###autoload
(defun +magit-optimize-process-calls-h ()
  (when-let (path (executable-find magit-git-executable t))
    (setq-local magit-git-executable path)))

;;;###autoload
(defun +magit-reveal-point-if-invisible-h ()
  "Reveal the point if in an invisible region."
  (if (derived-mode-p 'org-mode)
      (org-reveal '(4))
    (require 'reveal)
    (reveal-post-command)))

;;;###autoload
(defun +magit--set-window-state-h ()
  (setq-local +magit--pos (list (current-buffer) (point) (window-start))))

;;;###autoload
(defun +magit--restore-window-state-h ()
  (when (and +magit--pos (eq (current-buffer) (car +magit--pos)))
    (goto-char (cadr +magit--pos))
    (set-window-start nil (caddr +magit--pos) t)
    (kill-local-variable '+magit--pos)))

;;;###autoload
(defun +magit-buffer-p (buf)
  (with-current-buffer buf
    (and (derived-mode-p 'magit-mode)
         (not (eq major-mode 'magit-process-mode)))))

;;;###autoload
(defun +magit-enlargen-fringe-h ()
  "Make fringe larger in magit."
  (and (display-graphic-p)
       (derived-mode-p 'magit-section-mode)
       +magit-fringe-size
       (let ((left  (or (car-safe +magit-fringe-size) +magit-fringe-size))
             (right (or (cdr-safe +magit-fringe-size) +magit-fringe-size)))
         (set-window-fringes nil left right))))

;;;###autoload
(defun +vc-gutter-init-maybe-h ()
  "Enable `git-gutter-mode' in the current buffer.
If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (cond
     ((and (file-remote-p (or file-name default-directory))
           (not +vc-gutter-in-remote-files)))
     ;; UX: If not a valid file, wait until it is written/saved to activate
     ;;   git-gutter.
     ((not (and file-name (vc-backend file-name)))
      (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local))
     ;; UX: Allow git-gutter or git-gutter-fringe to activate based on the
     ;;   type of frame we're in. This allows git-gutter to work for silly
     ;;   geese who open both tty and gui frames from the daemon.
     ((if (and (display-graphic-p)
               (require 'git-gutter-fringe nil t))
          (setq-local git-gutter:init-function      #'git-gutter-fr:init
                      git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                      git-gutter:clear-function     #'git-gutter-fr:clear
                      git-gutter:window-width -1)
        (setq-local git-gutter:init-function      'nil
                    git-gutter:view-diff-function #'git-gutter:view-diff-infos
                    git-gutter:clear-function     #'git-gutter:clear-diff-infos
                    git-gutter:window-width 1))
      (unless (memq major-mode git-gutter:disabled-modes)
        (git-gutter-mode +1)
        (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local))))))

;;;###autoload
(defun +vc-gutter-update-h (&rest _)
  "Refresh git-gutter on ESC. Return nil to prevent shadowing other
`doom-escape-hook' hooks."
  (ignore (or (memq this-command '(git-gutter:stage-hunk
                                   git-gutter:revert-hunk))
              inhibit-redisplay
              (if git-gutter-mode
                  (git-gutter)
                (+vc-gutter-init-maybe-h)))))
