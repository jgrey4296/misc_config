;;; editor/format/autoload.el -*- lexical-binding: t; -*-

(defvar +format-region-p nil
  "Is non-nil if currently reformatting a selected region, rather than the whole
buffer.")

;; This must be redefined here because `format-all' only makes it available at
;; compile time.
(defconst +format-system-type
  (cl-case system-type
    (windows-nt 'windows)
    (cygwin     'windows)
    (darwin     'macos)
    (gnu/linux  'linux)
    (berkeley-unix
     (save-match-data
       (let ((case-fold-search t))
         (cond ((string-match "freebsd" system-configuration) 'freebsd)
               ((string-match "openbsd" system-configuration) 'openbsd)
               ((string-match "netbsd"  system-configuration) 'netbsd))))))
  "Current operating system according to the format-all package.")

(defun +format--resolve-system (choices)
  "Get first choice matching `format-all-system-type' from CHOICES."
  (cl-loop for choice in choices
           if (atom choice) return choice
           else if (eql +format-system-type (car choice))
           return (cadr choice)))

(defun +format--make-command (formatter &rest _)
  `(format-all--buffer-thunk
    (lambda (input)
      (with-silent-modifications
        (setq buffer-file-name ,(buffer-file-name (buffer-base-buffer))
              default-directory ,default-directory)
        (delay-mode-hooks (funcall ',major-mode))
        (insert input)
        (condition-case e
            (progn
              (doom-log "formatter (commandp) %s" #',formatter)
              (call-interactively #',formatter)
              (list nil ""))
          (error (list t (error-message-string e))))))))

(defun +format--make-function (formatter &rest _)
  `(progn
     (doom-log "formatter (functionp) %s" #',formatter)
     (format-all--buffer-thunk #',formatter)))

(defun +format--make-shell-command (command ok-statuses error-regexp)
  (+format--make-shell-command-list (split-string command " " t)
                                    ok-statuses error-regexp))

(defun +format--make-shell-command-list (command-list ok-statuses error-regexp)
  `(let (args)
     (dolist (arg ',command-list)
       (cond ((stringp arg)
              (push arg args))
             ((listp arg)
              (catch 'skip
                (let (subargs this)
                  (while (setq this (pop arg))
                    (cond ((not (stringp (car arg)))
                           (let ((val (eval (pop arg) t)))
                             (unless val (throw 'skip nil))
                             (push (format this val) subargs)))
                          ((stringp this)
                           (push this subargs))))
                  (setq args (append subargs args)))))))
     (doom-log "formatter (arglist) %s" args)
     (if ,(and (or ok-statuses error-regexp) t)
         (apply #'format-all--buffer-hard
                ',ok-statuses ,error-regexp nil
                (reverse args))
       (apply #'format-all--buffer-easy (reverse args)))))

(cl-defun +format--set (name &key function modes unset)
  (declare (indent defun))
  (when (and unset (not (gethash name format-all--format-table)))
    (error "'%s' formatter does not exist to be unset" name))
  (puthash name function format-all--format-table)
  (dolist (mode (ensure-list modes))
    (cl-destructuring-bind (m &optional probe)
        (ensure-list mode)
      (if unset
          (puthash m (assq-delete-all name (gethash key format-all-mode-table))
                   format-all-mode-table)
        (format-all--pushhash
         m (cons name (if probe `(lambda () ,probe)))
         format-all--mode-table)))))


(defun +format--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function.

Stolen shamelessly from go-mode"
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        ((delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

;;;###autoload
(defun +format--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer.

Stolen shamelessly from go-mode"
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0)
        (column (current-column)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in +format--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (1- (- from line-offset)))
                (cl-incf line-offset len)
                (+format--delete-whole-line len)))
             ((error "Invalid rcs patch or internal error in +format--apply-rcs-patch")))))))
    (move-to-column column)))

(defun +format--current-indentation ()
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (current-indentation)))


;;
;; Public library

(defun +format-completing-read ()
  "TODO"
  (require 'format-all)
  (let* ((fmtlist (mapcar #'symbol-name (hash-table-keys format-all--format-table)))
         (fmt (completing-read "Formatter: " fmtlist)))
    (if fmt (intern fmt))))

;;;###autoload
(defun +format-probe-a (fn)
  "Use `+format-with' instead, if it is set.
Prompts for a formatter if universal arg is set."
  (cond ((or buffer-read-only (eq +format-with :none))
         (list nil nil))
        (current-prefix-arg
         (list (or (+format-completing-read)
                   (user-error "Aborted"))
               t))
        (+format-with
         (list +format-with t))
        ((and +format-with-lsp
              (bound-and-true-p lsp-managed-mode)
              (lsp-feature? "textDocument/formatting"))
         (list 'lsp nil))
        ((and +format-with-lsp
              (bound-and-true-p eglot--managed-mode)
              (eglot--server-capable :documentFormattingProvider))
         (list 'eglot nil))
        ((funcall fn))))

;;;###autoload
(defun +format-buffer-a (formatter mode-result)
  "Advice that extends `format-all-buffer--with' to:

1. Enable partial/region reformatting, while preserving leading indentation,
2. Applies changes via RCS patch, line by line, to protect buffer markers and
   reduce cursor movement or window scrolling.

See `+format/buffer' for the interactive version of this function, and
`+format-buffer-h' to use as a `before-save-hook' hook."
  (cond
   ((eq formatter 'lsp)
    (call-interactively
     (if +format-region-p #'lsp-format-region #'lsp-format-buffer)))
   ((eq formatter 'eglot)
    (call-interactively
     (if +format-region-p #'eglot-format #'eglot-format-buffer)))
   ((let ((f-function (gethash formatter format-all--format-table))
          (executable (format-all--formatter-executable formatter))
          (indent 0)
          (old-line-number (line-number-at-pos))
          (old-column (current-column)))
      (pcase-let*
          ((`(,output ,errput)
            ;; To reliably format regions, rather than the whole buffer, and
            ;; `format-all' (and various formatting functions, like `gofmt') widen
            ;; the buffer, we must copy the region first.
            (let ((output (buffer-substring-no-properties (point-min) (point-max)))
                  (origin-buffer (or (buffer-base-buffer) (current-buffer)))
                  ;; Fixes #5133: some packages (like lsp-mode) can do a bunch
                  ;; of complicated stuff in these hooks. Better to not have to
                  ;; deal with any of them at all.
                  write-file-functions
                  before-save-hook
                  after-save-hook
                  kill-buffer-query-functions
                  kill-buffer-hook)
              (with-temp-buffer
                (with-silent-modifications
                  (insert output)
                  ;; Ensure this temp buffer seems as much like the origin
                  ;; buffer as possible, in case the formatter is an elisp
                  ;; function, like `gofmt'.
                  (cl-loop for (var . val)
                           in (cl-remove-if-not #'listp (buffer-local-variables origin-buffer))
                           ;; Making enable-multibyte-characters buffer-local
                           ;; causes an error.
                           unless (eq var 'enable-multibyte-characters)
                           ;; Fixes #5133: don't deal with complicated hook
                           ;; functionality! This isn't a real buffer anyway.
                           unless (string-match-p (symbol-name var) "-\\(hook\\|functions\\)$")
                           ;; Using setq-local would quote var.
                           do (set (make-local-variable var) val))
                  ;; Since we're piping a region of text to the formatter, remove
                  ;; any leading indentation to make it look like a file.
                  (setq indent (+format--current-indentation))
                  (when (> indent 0)
                    (indent-rigidly (point-min) (point-max) (- indent)))
                  (funcall f-function executable mode-result)))))
           (`,status
            (cond ((null output) :error)
                  ((eq output t) :already-formatted)
                  (t :reformatted))))
        (unwind-protect
            (when (eq status :reformatted)
              (let ((tmpfile (make-temp-file "doom-format"))
                    (patchbuf (get-buffer-create " *doom format patch*"))
                    (coding-system-for-read coding-system-for-read)
                    (coding-system-for-write coding-system-for-write))
                (unless IS-WINDOWS
                  (setq coding-system-for-read 'utf-8
                        coding-system-for-write 'utf-8))
                (unwind-protect
                    (progn
                      (with-current-buffer patchbuf
                        (erase-buffer))
                      (with-temp-file tmpfile
                        (erase-buffer)
                        (insert output)
                        (when (> indent 0)
                          ;; restore indentation without affecting new
                          ;; indentation
                          (indent-rigidly (point-min) (point-max)
                                          (max 0 (- indent (+format--current-indentation))))))
                      (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                          (setq status :already-formatted)
                        (+format--apply-rcs-patch patchbuf)
                        (list output errput)))
                  (kill-buffer patchbuf)
                  (delete-file tmpfile))))
          (format-all--show-or-hide-errors errput)
          (goto-char (point-min))
          (forward-line (1- old-line-number))
          (let ((line-length (- (point-at-eol) (point-at-bol))))
            (goto-char (+ (point) (min old-column line-length))))
          (run-hook-with-args 'format-all-after-format-functions formatter status)
          (message (pcase status
                     (:error "Formatting error")
                     (:already-formatted "Already formatted")
                     (:reformatted (format "Reformatted with %s" formatter))))))))))


;;
;;; Commands

(defun +format--org-region (beg end)
  "Reformat the region within BEG and END.
If nil, BEG and/or END will default to the boundaries of the src block at point."
  (let ((element (org-element-at-point)))
    (save-excursion
      (let* ((block-beg (save-excursion
                          (goto-char (org-babel-where-is-src-block-head element))
                          (line-beginning-position 2)))
             (block-end (save-excursion
                          (goto-char (org-element-property :end element))
                          (skip-chars-backward " \t\n")
                          (line-beginning-position)))
             (beg (if beg (max beg block-beg) block-beg))
             (end (if end (min end block-end) block-end))
             (lang (org-element-property :language element))
             (major-mode (org-src-get-lang-mode lang)))
        (if (eq major-mode 'org-mode)
            (user-error "Cannot reformat an org src block in org-mode")
          (+format/region beg end))))))

(defun +format--buffer ()
  (if (and (eq major-mode 'org-mode)
           (org-in-src-block-p t))
      (+format--org-region (point-min) (point-max))
    (if (called-interactively-p 'any)
        (format-all-buffer)
      (ignore-errors (format-all-buffer)))))

;;;###autoload
(defun +format/buffer ()
  "Reformat the current buffer using LSP or `format-all-buffer'."
  (interactive)
  (+format--buffer))

;;;###autoload
(defun +format/region (beg end)
  "Runs the active formatter on the lines within BEG and END.

WARNING: this may not work everywhere. It will throw errors if the region
contains a syntax error in isolation. It is mostly useful for formatting
snippets or single lines."
  (interactive "rP")
  (let ((+format-region-p t))
    (save-restriction
      (narrow-to-region beg end)
      (+format--buffer))))

;;;###autoload
(defun +format/region-or-buffer ()
  "Runs the active formatter on the selected region (or whole buffer, if nothing
is selected)."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'+format/region
     #'+format/buffer)))


;;
;; Hooks

;;;###autoload
(defalias '+format-buffer-h #'+format/buffer
  "Format the source code in the current buffer with minimal feedback.

Meant for `before-save-hook'.")


;; Allow a specific formatter to be used by setting `+format-with', either
;; buffer-locally or let-bound.
;;;###autoload
(advice-add 'format-all--probe :around #'+format-probe-a)

;; Doom uses a modded `format-all-buffer', which
;;   1. Enables partial reformatting (while preserving leading indentation),
;;   2. Applies changes via RCS patch, line by line, to protect buffer markers
;;      and avoid any jarring cursor+window scrolling.
(advice-add 'format-all-buffer--with :override #'+format-buffer-a)

;; format-all-mode "helpfully" raises an error when it doesn't know how to
;; format a buffer.
(add-to-list 'debug-ignored-errors "^Don't know how to format ")

;; Don't pop up imposing warnings about missing formatters, but still log it in
;; to *Messages*.
(defadvice! +format--all-buffer-from-hook-a (fn &rest args)
  :around #'format-all-buffer--from-hook
  (letf! (defun format-all-buffer--with (formatter mode-result)
           (when (or (eq formatter 'lsp)
                     (eq formatter 'eglot)
                     (condition-case-unless-debug e
                         (format-all--formatter-executable formatter)
                       (error
                        (message "Warning: cannot reformat buffer because %S isn't installed"
                                 (gethash formatter format-all--executable-table))
                        nil)))
             (funcall format-all-buffer--with formatter mode-result)))
    (apply fn args)))


;;;###autoload (autoload 'format-all--probe "format-all")
