;; -*- mode:emacs-lisp; -*- lexical-bindings: t; -*-
;;
;;; Main commands
(require 'xref)

(defvar +jg-lookup-valid-keywords '(
                                    :definition
                                    :declaration
                                    :implementations
                                    :type-definition
                                    :references
                                    :documentation
                                    :assignments
                                    )
  "Valid Types of Lookup commands that can be registered")

(defun +lookup-thing-at-point (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point. "
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((evil-visual-state-p)
         (buffer-substring-no-properties evil-visual-beginning evil-visual-end))
        ((region-active-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((and (not (null thing)) (symbolp thing))
         (thing-at-point thing t))
        ((memq (xref-find-backend) '(eglot elpy nox))
         (thing-at-point 'symbol t))
        (t
         (xref-backend-identifier-at-point (xref-find-backend)))
        )
  )

;;;###autoload
(defun +lookup/definition (identifier &optional arg)
  "Jump to the definition of IDENTIFIER (defaults to the symbol at point).

Each function in `+lookup-definition-functions' is tried until one changes the
point or current buffer. Falls back to dumb-jump, naive
ripgrep/the_silver_searcher text search, then `evil-goto-definition' if
evil-mode is active."
  (interactive (list (+lookup-thing-at-point) current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :definition identifier nil arg))
        ((user-error "Couldn't find the definition of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/declaration (identifier &optional arg)
  (interactive (list (+lookup-thing-at-point) current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :declaration identifier nil arg))
        ((user-error "Couldn't find the declaration of %S" (substring-no-properties identifier))))
  )

;;;###autoload
(defun +lookup/implementations (identifier &optional arg)
  "Jump to the implementations of IDENTIFIER (defaults to the symbol at point).

Each function in `+lookup-implementations-functions' is tried until one changes
the point or current buffer."
  (interactive (list (+lookup-thing-at-point) current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :implementations identifier nil arg))
        ((user-error "Couldn't find the implementations of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/type-definition (identifier &optional arg)
  "Jump to the type definition of IDENTIFIER (defaults to the symbol at point).

Each function in `+lookup-type-definition-functions' is tried until one changes
the point or current buffer."
  (interactive (list (+lookup-thing-at-point) current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :type-definition identifier nil arg))
        ((user-error "Couldn't find the definition of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/references (identifier &optional arg)
  "Show a list of usages of IDENTIFIER (defaults to the symbol at point)

Tries each function in `+lookup-references-functions' until one changes the
point and/or current buffer. Falls back to a naive ripgrep/the_silver_searcher
search otherwise."
  (interactive (list (+lookup-thing-at-point) current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :references identifier nil arg))
        ((user-error "Couldn't find references of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/documentation (identifier &optional arg)
  "Show documentation for IDENTIFIER (defaults to symbol at point or selection.

First attempts the :documentation handler specified with `set-lookup-handlers!'
for the current mode/buffer (if any), then falls back to the backends in
`+lookup-documentation-functions'."
  (interactive (list (+lookup-thing-at-point) current-prefix-arg))
  (+lookup--run-handler :documentation identifier)
  )

;;;###autoload
(defun +lookup/assignments (identifier &optional arg)
  (interactive (list (+lookup-thing-at-point) current-prefix-arg))
  (cond ((+lookup--jump-to :assignments identifier #'pop-to-buffer arg))
        ((user-error "Couldn't find assignments for %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/file (&optional path)
  "Figure out PATH from whatever is at point and open it.

Each function in `+lookup-file-functions' is tried until one changes the point
or the current buffer.

Otherwise, falls back on `find-file-at-point'."
  (interactive)
  (cond ((and path
              buffer-file-name
              (file-equal-p path buffer-file-name)
              (user-error "Already here")))
        ((+lookup--jump-to :file path))
        ((user-error "Couldn't find any files here")))
  )

;;;###autoload
(defun +lookup/choose (identifier &optional arg)
  (interactive (list (+lookup-thing-at-point) current-prefix-arg))
  (let ((handler-sym (intern (ivy-read "Handler Option: " +jg-lookup-valid-keywords)))
        )
    (cond ((+lookup--jump-to handler-sym identifier #'pop-to-buffer arg))
          ((user-error "Failed to use %s for %S" handler-sym (substring-no-properties identifier))))
    )
  )

(defun +lookup--run-handler (prop identifier)
  (let* ((handlers (pcase prop
                     (:definition      +lookup-definition-functions)
                     (:declaration     +lookup-declaration-functions)
                     (:implementations +lookup-implementations-functions)
                     (:type-definition +lookup-type-definition-functions)
                     (:references      +lookup-references-functions)
                     (:documentation   +lookup-documentation-functions)
                     (:file            +lookup-file-functions)
                     (:assignments     +lookup-assignments-functions)
                     (_ (user-error "Unrecognized lookup prop" prop))
                     ))
         selected-handler
         )
    ;; Select just one handler:
    (pcase handlers
      ('nil (user-error "No Handler Found for: %s" prop))
      ((and (pred listp) (pred (lambda (x) (< 1 (length x)))))
       ;; (setq selected-handler (intern-soft (ivy-read "Select a handler: " handlers :require-match t)))
       (setq selected-handler (car handlers))
       )
      ((pred listp)
       (setq selected-handler (car handlers)))
      (_ (setq selected-handler handlers))
      )
    ;; Run the Handler:
    (if (commandp selected-handler)
        (call-interactively selected-handler)
      (funcall selected-handler identifier))
    )
  )

(defun +lookup--jump-to (prop identifier &optional display-fn arg)
  (let ((origin (point-marker))
        (result (+lookup--run-handler prop identifier))
        )
    ;; Deal with result
    (unwind-protect
        (when (cond ((null result)
                     (message "No lookup handler could find %S" identifier)
                     nil)
                    ((markerp result)
                     (funcall (or display-fn #'switch-to-buffer)
                              (marker-buffer result))
                     (goto-char result)
                     result)
                    (result))
          (with-current-buffer (marker-buffer origin)
            (better-jumper-set-jump (marker-position origin)))
          result)
      (set-marker origin nil))
    )
  )

;;;###autoload
(defun +jg-lookup-debug-settings ()
  (interactive)
  (let ((handlers (list
                   (cons :assignments     +lookup-assignments-functions)
                   (cons :definition      +lookup-definition-functions)
                   (cons :declaration     +lookup-declaration-functions)
                   (cons :documentation   +lookup-documentation-functions)
                   (cons :file            +lookup-file-functions)
                   (cons :implementations +lookup-implementations-functions)
                   (cons :references      +lookup-references-functions)
                   (cons :type-definition +lookup-type-definition-functions)
                   ))
        )
    (message "Lookup Handlers Are:\n%s"
             (string-join (mapcar #'(lambda (x)
                                      (format "%-25s : %s" (car x) (cdr x)))
                                  handlers) "\n")
             )
    )
  )
