;;
;; Functions to enable unit testing an org file
;; TODO: put parse-tests and run-tests in variables,
;; and use a macro to expand them, allowing the two to be defined together
;; simplifying the addition of new tests
;;
(cl-defstruct jg-org-test-extracts start end heading link text)
(cl-defstruct jg-org-test-group name tests link bound start)
(cl-defstruct jg-org-test type locator value)
;; Current test types:
'(:section-check :length-check :order-check :citation-check :mention-check :codeblock-check :tag-check)
(cl-defstruct jg-org-test-results name results link)

(defvar jg-org-test-heading-regexp "^*+ %s$"
  "Regexp to format when searching for a location")

(defvar jg-org-test-src-block-regexp "^ +#\\+begin_src"
  "Regexp to find src blocks")

(defun jg-org-test-to-string (test)
  (let ((tt (jg-org-test-type test))
        (loc (jg-org-test-locator test))
        (val (jg-org-test-value test))
        )
    (cond
     ((eq tt :section-check ) (format "%s should have section %s." loc val))
     ((eq tt :length-check ) (format "%s should be %s than %s %s." loc (car val) (cadr val) (caddr val)))
     ((eq tt :order-check ) (format "%s should precede %s." loc val))
     ((eq tt :citation-check ) (format "%s should cite:%s." loc (if (listp val) (mapconcat 'identity val ",") val)))
     ((eq tt :mention-check ) (format "%s should mention \"%s\"." loc val))
     ((eq tt :codeblock-check ) (format "%s should have a codeblock%s." loc (if val (format "in %s" val) "")))
     ((eq tt :tag-check)   (format "%s should have tag \"%s\"." loc val))
     (t (error "unrecognized test type"))
     )
    )
  )

;; Main Access
(defun jg-org-test-org-file ()
  (interactive)
  ;; check file is in org mode
  (assert (eq major-mode 'org-mode))
  ;; get tests
  (let* ((test-texts (seq-filter 'identity (jg-org-test-find-tests)))
         (parsed-tests (seq-filter 'identity (-flatten (mapcar 'jg-org-test-parse-tests test-texts))))
         (test-results (jg-org-test-run-tests parsed-tests))
         )
    ;; print test results
    (jg-org-test-print-results test-results)
    )
  )
