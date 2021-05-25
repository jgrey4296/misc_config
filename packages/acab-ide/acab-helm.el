;;; ../../../Volumes/documents/github/emacs_files/packages/acab-ide/acab-helm.el -*- lexical-binding: t; -*-

;;Helm
(defun acab-ide/get-rule-helm-candidates ()
  "Generate candidates for rule helm"
  (hash-table-keys acab-ide/rules)
  )
(defun acab-ide/find-rule (x)
  (message "Get rules: %s" (helm-marked-candidates))
  ;;TODO find rules
  ;;Reacab-ideve rules
  ;;update working ruleset
  ;;open rule files
  (let ((buf (if (get-buffer x) x (find-file x)))
        (window (if (window-valid-p (plist-get acab-ide/window-configuration :rule))
                    (plist-get acab-ide/window-configuration :rule)
                  (selected-window)))
        )
    (window--display-buffer buf window 'window)
    )
  )
(defun acab-ide/create-rule (x)
  (message "Creating Rule: %s" x)
  ;;TODO create rule
  ;;update working ruleset
  ;;open rule file
  ;;insert template
  ;;update rule hashtable
  (let* ((acab-ide-snippet-rule-name x)
         (file-string (format "%s.rule" (s-replace "." "_" x)))
         (rule-path (f-join acab-ide/ide-data-loc "rules" file-string))
         (window (if (window-valid-p (plist-get acab-ide/window-configuration :rule))
                     (plist-get acab-ide/window-configuration :rule)
                   (selected-window)))
         )

    (window--display-buffer (generate-new-buffer x) window 'window)
    (with-current-buffer x
      (acab-ide-mode)
      (yas-expand-snippet (yas-lookup-snippet "rule")
                          (point-min) (point-max))
      (write-file rule-path)
      (puthash x rule-path acab-ide/rules)
      )
    )
  )

(defun acab-ide/get-type-helm-candidates ()
  (hash-table-keys acab-ide/types)
)
(defun acab-ide/find-type (x) )
(defun acab-ide/create-type (x) )

(defun acab-ide/get-pattern-helm-candidates ()
  (hash-table-keys acab-ide/patterns)
  )
(defun acab-ide/find-pattern (x) )
(defun acab-ide/create-pattern (x) )

(defun acab-ide/get-test-helm-candidates ()
  (hash-table-keys acab-ide/tests)
)
(defun acab-ide/find-test (x) )
(defun acab-ide/create-test (x) )

(defun acab-ide/get-sentence-helm-candidates ()
  acab-ide/sentences
)
(defun acab-ide/add-sentence () )
(defun acab-ide/add-new-sentence () )

(defun acab-ide/get-crosscut-helm-candidates ()
  (hash-table-keys acab-ide/crosscuts)
)
(defun acab-ide/add-crosscut ())

(defun acab-ide/get-tag-helm-candidates ()
  acab-ide/tags
)
(defun acab-ide/toggle-tag (x)
  ;;find tags
  ;;remove selected from existing
  ;;add remaining
)
(defun acab-ide/create-tag(x)
  ;;find tags
  ;;add selected
)


(defun acab-ide/get-channel-helm-candidates ()
  acab-ide/channels
)
(defun acab-ide/create-channel () )
(defun acab-ide/find-channel ())
