;; -*- mode:emacs-lisp; -*- lexical-bindings: t; -*-
;;
;;; Main commands
(defvar +jg-lookup-valid-keywords '(
                                    :definition
                                    :implementations
                                    :type-definition
                                    :references
                                    :documentation
                                    :assignments
                                    )
  "Valid Types of Lookup commands that can be registered")


;;;###autoload
(defun +lookup/definition (identifier &optional arg)
  "Jump to the definition of IDENTIFIER (defaults to the symbol at point).

Each function in `+lookup-definition-functions' is tried until one changes the
point or current buffer. Falls back to dumb-jump, naive
ripgrep/the_silver_searcher text search, then `evil-goto-definition' if
evil-mode is active."
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :definition identifier nil arg))
        ((user-error "Couldn't find the definition of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/implementations (identifier &optional arg)
  "Jump to the implementations of IDENTIFIER (defaults to the symbol at point).

Each function in `+lookup-implementations-functions' is tried until one changes
the point or current buffer."
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :implementations identifier nil arg))
        ((user-error "Couldn't find the implementations of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/type-definition (identifier &optional arg)
  "Jump to the type definition of IDENTIFIER (defaults to the symbol at point).

Each function in `+lookup-type-definition-functions' is tried until one changes
the point or current buffer."
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :type-definition identifier nil arg))
        ((user-error "Couldn't find the definition of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/references (identifier &optional arg)
  "Show a list of usages of IDENTIFIER (defaults to the symbol at point)

Tries each function in `+lookup-references-functions' until one changes the
point and/or current buffer. Falls back to a naive ripgrep/the_silver_searcher
search otherwise."
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :references identifier nil arg))
        ((user-error "Couldn't find references of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/documentation (identifier &optional arg)
  "Show documentation for IDENTIFIER (defaults to symbol at point or selection.

First attempts the :documentation handler specified with `set-lookup-handlers!'
for the current mode/buffer (if any), then falls back to the backends in
`+lookup-documentation-functions'."
  (interactive (list (doom-thing-at-point-or-region) current-prefix-arg))
  (cond ((+lookup--jump-to :documentation identifier #'pop-to-buffer arg))
        ((user-error "Couldn't find documentation for %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/assignments (identifier &optional arg)
  (interactive (list (doom-thing-at-point-or-region) current-prefix-arg))
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

        ((user-error "Couldn't find any files here"))))

(defun +lookup--run-handler (handler identifier)
  (if (commandp handler)
      (call-interactively handler)
    (funcall handler identifier)))

(defun +lookup--run-handlers (handler identifier origin)
  (doom-log "Looking up '%s' with '%s'" identifier handler)
  (condition-case-unless-debug e
      (let ((wconf (current-window-configuration))
            (result (condition-case-unless-debug e
                        (+lookup--run-handler handler identifier)
                      (error
                       (doom-log "Lookup handler %S threw an error: %s" handler e)
                       'fail))))
        (cond ((eq result 'fail)
               (set-window-configuration wconf)
               nil)
              ((or (get handler '+lookup-async)
                   (eq result 'deferred)))
              ((or result
                   (null origin)
                   (/= (point-marker) origin))
               (prog1 (point-marker)
                 (set-window-configuration wconf)))))
    ((error user-error)
     (message "Lookup handler %S: %s" handler e)
     nil)))

(defun +lookup--jump-to (prop identifier &optional display-fn arg)
  (let* ((origin (point-marker))
         (handlers (pcase prop
                     (:definition      +lookup-definition-functions)
                     (:implementations +lookup-implementations-functions)
                     (:type-definition +lookup-type-definition-functions)
                     (:references      +lookup-references-functions)
                     (:documentation   +lookup-documentation-functions)
                     (:file            +lookup-file-functions)
                     (:assignments     +lookup-assignments-functions)
                     (_ (user-error "Unrecognized lookup prop" prop))
                     ))
         selected-handler
         result
         )
    ;; Select just one handler:
    (pcase handlers
      ((guard arg)
       (user-error "TODO: handle arg"))
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
    (setq result (+lookup--run-handler selected-handler identifier))
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
      (set-marker origin nil))))

;;;###autoload
(defun +jg-lookup-debug-settings ()
  (interactive)
  (let ((handlers (list (cons :definition      +lookup-definition-functions)
                        (cons :implementations +lookup-implementations-functions)
                        (cons :type-definition +lookup-type-definition-functions)
                        (cons :references      +lookup-references-functions)
                        (cons :documentation   +lookup-documentation-functions)
                        (cons :file            +lookup-file-functions)
                        (cons :assignments     +lookup-assignments-functions)
                        ))
        )
    (message "Lookup Handlers Are:\n%s"
             (string-join (mapcar #'(lambda (x)
                                      (format "%-25s : %s" (car x) (cdr x)))
                                  handlers) "\n")
             )
    )
  )
