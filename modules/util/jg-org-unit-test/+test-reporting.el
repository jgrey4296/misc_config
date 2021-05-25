;; Test Reporting
(defun jg-org-test-print-results (results)
  (with-temp-buffer-window "*Test Results*"
                           nil
                           nil
                           (princ "* Test Results\n")
                           (mapc 'jg-org-test-print-test-results results)
                           (org-mode)
                           )
  (with-current-buffer "*Test Results*"
    (org-mode)
    (goto-char (point-min))
    (org-cycle)
    )
  )
(defun jg-org-test-print-test-results (testgroup)
  (let ((successes (reduce '+ (mapcar (lambda (x) (if (cdr x) 1 0)) (jg-org-test-results-results testgroup))))
        (total (length (jg-org-test-results-results testgroup))))
    (assert (jg-org-test-results-p testgroup))
    (princ (format "** %s: (%s / %s) \n" (jg-org-test-results-name testgroup) successes total))
    (princ (format "    %s\n" (jg-org-test-results-link testgroup)))
    (mapc (lambda (x) (princ (s-concat "    " (if (cdr x) "✓" "X") " : " (car x) "\n"))) (jg-org-test-results-results testgroup))
    )
  )

