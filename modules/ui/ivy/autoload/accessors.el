;;; accessors.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defvar jg-ivy-all-major-modes nil "List of all major modes")

(defvar jg-ivy-all-minor-modes nil "List of all minor modes")

;;;###autoload
(defun +jg-ivy-buffer ()
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :predicate           #'jg-ivy-buffer-predicate-p
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action              #'ivy--switch-buffer-action
            :matcher             #'ivy--switch-buffer-matcher
            :sort t
            :caller 'ivy-switch-buffer)
  )

;;;###autoload
(defun +ivy-yas-prompt-fn (prompt choices &optional display-fn)
  (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))

;;;###autoload
(defun +ivy/projectile-find-file ()
  "A more sensible `counsel-projectile-find-file', which will revert to
`counsel-find-file' if invoked from $HOME or /, `counsel-file-jump' if invoked
from a non-project, `projectile-find-file' if in a big project (more than
`ivy-sort-max-size' files), or `counsel-projectile-find-file' otherwise.

The point of this is to avoid Emacs locking up indexing massive file trees."
  (interactive)
  ;; Spoof the command so that ivy/counsel will display the (well fleshed-out)
  ;; actions list for `counsel-find-file' on C-o. The actions list for the other
  ;; commands aren't as well configured or are empty.
  (let ((this-command 'counsel-find-file))
    (call-interactively
     (cond ((or (file-equal-p default-directory "~")
                (file-equal-p default-directory "/")
                (when-let (proot (projectile-project-root))
                  (file-equal-p proot "~")))
            #'counsel-find-file)

           ((projectile-project-p)
            (let ((files (projectile-current-project-files)))
              (if (<= (length files) ivy-sort-max-size)
                  #'counsel-projectile-find-file
                #'projectile-find-file)))

           (#'counsel-file-jump)))))

;;;###autoload
(defun +ivy/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.

If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+ivy-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
(defun +ivy/project-search-from-cwd (&optional arg initial-query)
  "Performs a project search recursively from the current directory.

If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+ivy-file-search :query initial-query
                    :in default-directory
                    :all-files arg
                    :prompt (format "Search Directory [%s]: " (f-base default-directory)))
  )

(defun +ivy--switch-buffer (workspace other)
  (let ((current (not other))
        prompt action filter update unwind)
    (cond ((and workspace current)
           (setq prompt "Switch to workspace buffer: "
                 action #'ivy--switch-buffer-action
                 filter #'+ivy--is-workspace-other-buffer-p))
          (workspace
           (setq prompt "Switch to workspace buffer in other window: "
                 action #'ivy--switch-buffer-other-window-action
                 filter #'+ivy--is-workspace-buffer-p))
          (current
           (setq prompt "Switch to buffer: "
                 action #'ivy--switch-buffer-action))
          ((setq prompt "Switch to buffer in other window: "
                 action #'ivy--switch-buffer-other-window-action)))
    (when +ivy-buffer-preview
      (cond ((not (and ivy-use-virtual-buffers
                       (eq +ivy-buffer-preview 'everything)))
             (setq update #'+ivy--switch-buffer-preview
                   unwind #'+ivy--switch-buffer-unwind))
            ((setq update #'+ivy--switch-buffer-preview-all
                   unwind #'+ivy--switch-buffer-unwind))))
    (ivy-read prompt 'internal-complete-buffer
              :action action
              :predicate filter
              :update-fn update
              :unwind unwind
              :preselect (buffer-name (other-buffer (current-buffer)))
              :matcher #'ivy--switch-buffer-matcher
              :keymap ivy-switch-buffer-map
              ;; NOTE A clever disguise, needed for virtual buffers.
              :caller #'ivy-switch-buffer)))

;;;###autoload
(defun +ivy/switch-workspace-buffer (&optional arg)
  "Switch to another buffer within the current workspace.

If ARG (universal argument), open selection in other-window."
  (interactive "P")
  (+ivy--switch-buffer t arg))

;;;###autoload
(defun +ivy/switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (+ivy--switch-buffer nil nil))

;;;###autoload
(defun +ivy/switch-buffer-other-window ()
  "Switch to another buffer in another window."
  (interactive)
  (+ivy--switch-buffer nil t))

;;;###autoload
(defun +ivy/woccur ()
  "Invoke a wgrep buffer on the current ivy results, if supported."
  (interactive)
  (unless (window-minibuffer-p)
    (user-error "No completion session is active"))
  (require 'wgrep)
  (let ((caller (ivy-state-caller ivy-last)))
    (if-let (occur-fn (plist-get +ivy-edit-functions caller))
        (ivy-exit-with-action
         (lambda (_) (funcall occur-fn)))
      (if-let (occur-fn (plist-get ivy--occurs-list caller))
          (let ((buffer (generate-new-buffer
                         (format "*ivy-occur%s \"%s\"*"
                                 (if caller (concat " " (prin1-to-string caller)) "")
                                 ivy-text))))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (funcall occur-fn))
              (setf (ivy-state-text ivy-last) ivy-text)
              (setq ivy-occur-last ivy-last)
              (setq-local ivy--directory ivy--directory))
            (ivy-exit-with-action
             `(lambda (_)
                (pop-to-buffer ,buffer)
                (ivy-wgrep-change-to-wgrep-mode))))
        (user-error "%S doesn't support wgrep" caller)))))

;;;###autoload
(defun +jg-ivy-similar-buffer ()
  """ Use Ivy to select buffer of the same mode """
  (interactive)
  (let* ((curr-mode major-mode)
         (proj (projectile-root-local default-directory))
         (buffs (-keep #'(lambda (x)
                           (when (equal curr-mode (buffer-local-value 'major-mode (get-buffer x)))
                             (cons (or (buffer-file-name x)
                                       (buffer-local-value 'default-directory x))
                                   x)
                             ))
                       (buffer-list)))
         )
  (ivy-read (format "%s Buffers: " curr-mode)
            buffs
            :action #'(lambda (x) (switch-to-buffer (cdr x)))
            :sort t
            :caller 'jg-ivy-similar-buffer
            )
  )
)

;;;###autoload
(defun +jg-ivy-change-major-mode (&optional arg)
  (interactive "P")
  (when (or arg (not jg-ivy-all-major-modes))
    (message "Building Major Mode List")
    (setq jg-ivy-all-major-modes nil)
    (cl-do-all-symbols (sym)
      (when (and (functionp sym)
                 (get sym 'derived-mode-parent))
        (push (symbol-name sym) jg-ivy-all-major-modes)
        )
      )
    (setq jg-ivy-all-major-modes (sort jg-ivy-all-major-modes #'string-lessp))
    )

  (let ((selected (ivy-read "Select Major Mode: " jg-ivy-all-major-modes)))
    (when (functionp (intern selected))
      (funcall (intern selected))
      )
    )
  )

;;;###autoload
(defun +jg-ivy-change-minor-mode (&optional arg)
  (interactive "P")
  (when (or arg (not jg-ivy-all-minor-modes))
    (message "Building Minor Mode List")
    (setq jg-ivy-all-minor-modes nil)
    (cl-do-all-symbols (sym)
      (when (and (functionp sym)
                 (s-suffix? "-minor-mode" (symbol-name sym)))
        (push (symbol-name sym) jg-ivy-all-minor-modes)
        )
      )
    (setq jg-ivy-all-minor-modes (sort jg-ivy-all-minor-modes #'string-lessp))
    )

  (let ((selected (ivy-read "Select Minor Mode: " jg-ivy-all-minor-modes)))
    (when (functionp (intern selected))
      (funcall (intern selected) 'toggle)
      )
    )
  )

;;;###autoload
(defun +jg-ivy-evil-registers (&optional arg)
  (interactive)
  (if (fboundp 'evil-register-list)
      (ivy-read "JG evil-registers: "
                (cl-loop for (key . val) in (evil-register-list)
                   collect (format "[%s]: %s"
                                   (propertize (char-to-string key)
                                               'face 'counsel-evil-register-face)
                                   (if (stringp val) val "")))
                :require-match t
                :initial-input "^"
                :action #'counsel-evil-registers-action
                :caller 'counsel-evil-registers)
    (user-error "Required feature `evil' not installed")))

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    March 22, 2025
;; Modified:   March 22, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; accessors.el ends here
