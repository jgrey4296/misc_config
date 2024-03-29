;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +web--fix-js-comments-h ()
      "Fix comment handling in `web-mode' for JavaScript."
      (when (member web-mode-content-type '("javascript" "jsx"))
        ;; For some reason the default is to insert HTML comments even
        ;; in JavaScript.
        (setq-local comment-start "//")
        (setq-local comment-end "")
        ;; Needed since otherwise the default value generated by
        ;; `comment-normalize-vars' will key off the syntax and think
        ;; that a single "/" starts a comment, which completely borks
        ;; auto-fill.
        (setq-local comment-start-skip "// *")))
