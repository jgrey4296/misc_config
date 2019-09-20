(defun doctest/find-tests ()
  ;; iterate through current org file,
  ;; extract any __doctest__ drawers contents
  (save-excursion
    (goto-char (point-min))
    (org-map-entries 'doctest/entry-bounds)
    )
  )

(defun doctest/extract-tests-from-entry ()
  (let ((start (point))
        end tests heading element)
    (setq heading (substring-no-properties (org-get-heading t t t t)))
    (save-excursion
      (org-end-of-subtree)
      (setq end (point))
      )
    (if (and (re-search-forward org-drawer-regexp end t)
             (s-equals? "__doctest__" (match-string 1))
             (s-equals? heading (org-get-heading t t t t)))
        (progn
          (setq element (cadr (org-element-at-point))
                text (buffer-substring-no-properties (plist-get element :contents-begin)
                                                     (plist-get element :contents-end)))
          `(,start ,end ,heading ,text)
          )
      `(,start ,end ,heading nil)
      )
    )
  )

(defun doctest/parse-tests (data)
  ;; split text into tests by full stops, defined groups,
  ;; and their originating heading
  ;; parse each test type
  (if (null (last data))
      nil
    (doctest/process-tests (nth 0 data)
                           (nth 1 data)
                           (nth 2 data)
                           (nth 3 data))
    )
  )

(defun doctest/process-tests (beg end heading text)
  ;; Where parsec parsing takes place

  )

(defun doctest/run-tests (tests)
  ;; run each test on the document
  ;; print results to temp buffer
  )

(defun doctest/run-test-X ()
  ;;separate test function for each form

  ;; document section exists
  ;; section subsection exists
  ;; section order check
  ;; section/subsection paragraph length bounds
  ;; section wordlcount
  ;; section subsection bounds

  ;; section citation check
  ;; section link check
  ;; section string check
  ;; section tag check
  ;; section codeblock check
  ;;section codeblock language check

  )

(defun doctest/print-results (results)
  (with-temp-buffer-window "*Test Results*"
                           nil
                           nil
                           ;;TODO: actually print the results
                           )
  )

(defun doctest/test-org-file ()
  (interactive)
  ;; check file is in org mode
  (assert (eq major-mode 'org-mode'))
  ;; get tests
  (let* ((test-texts (doctest/find-tests))
         (parsed-tests (seq-filter 'identity (mapcar 'doctest/parse-tests test-texts)))
         (test-results (doctest/run-tests parsed-tests))
         )
    ;; print test results
    (doctest/print-results test-results)
    )
)



