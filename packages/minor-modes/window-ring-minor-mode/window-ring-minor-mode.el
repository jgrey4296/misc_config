;;; window-ring-minor-mode/window-ring-minor-mode.el -*- lexical-binding: t; -*-
;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Windows.html
;; Window Queue: Display N windows on screen, and have the be
;; views of adjacent buffers in a list
;; Be able to move to the most-recent, oldest, or along the list
;;

;; To expand to vertical:
;; Add to ring means add a ring to the ring, and add buffer there

;; Ring_prime [ Sub-Ring1[a b ] Sub-Ring2[c d ] Sub-Ring3[e f ]]

;; Control: Add to new sub-ring, or add to top of current sub-ring
;; Display: 3 columns, middle column divided in 3

;; Add-to-list most-recent/oldest

(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name)))

(require 'evil)
(require 'persp-mode)
(require 'cl-lib)
(require 'window-ring--macros)
(require 'window-ring--util)
(require 'window-ring--edit)
(require 'window-ring--persp)
(require 'window-ring--movement)
(require 'window-ring--control)
(require 'window-ring--windows)

(defalias 'window-ring--get (symbol-function 'ring-ref))
(defalias 'window-ring--length (symbol-function 'ring-length))

;;-- vars

(defvar window-ring--adding nil)

(defvar window-ring-suppress-adding nil)

(defvar window-ring-buffer-test-fn 'identity "one argument, current buffer, return non-nil to add to current ring")

(defvar window-ring-column-fn #'window-ring-setup-columns-default "sets up the window config, returning windows to claim")

(defvar window-ring-name-suffix "-WR")

(defvar window-ring-focus-style 'newest "newest, oldest, balanced")

(defvar window-ring-selector 'top "using window-at-side-list")

;;-- end vars

;;-- mode

(define-minor-mode window-ring-minor-mode
  "A Minor Mode for easy control of a 3-ple view of a ring of buffers"
  :lighter "Window-Ring"
  :global t
  (add-to-list 'persp-created-functions #'window-ring-create-persp-fn)
  (add-to-list 'persp-activated-functions #'window-ring-activate-persp-fn)
  (add-to-list 'persp-before-deactivate-functions #'window-ring-deactivate-persp-fn)
  (add-to-list 'persp-before-kill-functions #'window-ring-kill-persp-fn)
  (add-hook 'find-file-hook #'window-ring-add-current-buffer)
  (add-hook 'kill-buffer-hook #'window-ring-remove-buffer)
  (add-hook 'kill-buffer-query-functions #'window-ring-protect-scratch-p -50)
  )

;;-- end mode

;;-- test predicates

(defun window-ring-p (&optional arg)
  (interactive "p")
  (when (persp-parameter 'window-ring)
    t)
  )

(defun window-ring-buffer-p (&optional arg buffer)
  (interactive "p")
  (when (and (window-ring-p)
             (ring-member (persp-parameter 'window-ring-actual)
                          (or buffer (current-buffer))))
    t)
  )

(defun window-ring-protect-scratch-p ()
  (not (with-window-ring
           (eq (current-buffer) wr-scratch)
         )
       )
  )

(defun window-ring-window-claimed-p (wind)
  (window-parameter wind 'window-ring-claimed)
  )

;;-- end test predicates


(provide 'window-ring-minor-mode)
