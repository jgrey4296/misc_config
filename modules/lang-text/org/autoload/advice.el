;;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org--server-visit-files-a (fn files &rest args)
  "Advise `server-visit-files' to load `org-protocol' lazily."
  ;; Wait until an org-protocol link is opened via emacsclient to load
  ;; `org-protocol'. Normally you'd simply require `org-protocol' and use it,
  ;; but the package loads all of org for no compelling reason, so...
  (if (not (cl-loop with protocol =
                    (if IS-WINDOWS
                        ;; On Windows, the file arguments for `emacsclient'
                        ;; get funnelled through `expand-file-path' by
                        ;; `server-process-filter'. This substitutes
                        ;; backslashes with forward slashes and converts each
                        ;; path to an absolute one. However, *all* absolute
                        ;; paths on Windows will match the regexp ":/+", so we
                        ;; need a more discerning regexp.
                        (regexp-quote
                         (or (bound-and-true-p org-protocol-the-protocol)
                             "org-protocol"))
                      ;; ...but since there is a miniscule possibility users
                      ;; have changed `org-protocol-the-protocol' I don't want
                      ;; this behavior for macOS/Linux users.
                      "")
                    for var in files
                    if (string-match-p (format "%s:/+" protocol) (car var))
                    return t))
      (apply fn files args)
    (require 'org-protocol)
    (apply #'org--protocol-detect-protocol-server fn files args)))

;;;###autoload
(advice-add 'server-visit-files  :around #'+org--server-visit-files-a)

;;;###autoload
(defun +org--fail-gracefully-a (&rest _)
  ;; HACK `org-id' doesn't check if `org-id-locations-file' exists or is
  ;;      writeable before trying to read/write to it.

    (file-writable-p org-id-locations-file))

;;;###autoload
(advice-add 'org-id-locations-save :before-while #'+org--fail-gracefully-a)
;;;###autoload
(advice-add 'org-id-locations-load :before-while #'+org--fail-gracefully-a)

;;;###autoload
(defun +org-inhibit-scrolling-a (fn &rest args)
    "Prevent the jarring scrolling that occurs when the-ToC is regenerated."
    (let ((p (set-marker (make-marker) (point)))
          (s (window-start)))
      (prog1 (apply fn args)
        (goto-char p)
        (set-window-start nil s t)
        (set-marker p nil))))

;;;###autoload
(advice-add 'toc-org-insert-toc :around #'+org-inhibit-scrolling-a)
