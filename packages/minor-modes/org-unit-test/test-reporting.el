;; Test Reporting

(defun org-unit-test-print-results (results)
  (with-temp-buffer-window "*Test Results*"
                           nil
                           nil
                           (princ "* Test Results\n")
                           (mapc 'org-unit-test-print-test-results results)
                           (org-mode)
                           )
  (with-current-buffer "*Test Results*"
    (org-mode)
    (goto-char (point-min))
    (org-cycle)
    )
  )

(defun org-unit-test-print-test-results (testgroup)
  (let ((successes (reduce '+ (mapcar (lambda (x) (if (cdr x) 1 0)) (org-unit-test-results-results testgroup))))
        (total (length (org-unit-test-results-results testgroup))))
    (cl-assert (org-unit-test-results-p testgroup))
    (princ (format "** %s: (%s / %s) \n" (org-unit-test-results-name testgroup) successes total))
    (princ (format "    %s\n" (org-unit-test-results-link testgroup)))
    (mapc (lambda (x) (princ (s-concat "    " (if (cdr x) "✓" "X") " : " (car x) "\n"))) (org-unit-test-results-results testgroup))
    )
  )
