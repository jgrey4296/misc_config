;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +company-abort-h ()
   "Don't persist company popups when switching back to normal mode.
   `company-box' aborts on mode switch so it doesn't need this.
   HACK `company-abort' doesn't no-op if company isn't active; causing
        unwanted side-effects, like the suppression of messages in the
        echo-area.
   REVIEW Revisit this to refactor; shouldn't be necessary!"
  (when company-candidates
    (company-abort))
  )
