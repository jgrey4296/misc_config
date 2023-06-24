
(require 'evil)
(require 'parsec)

(require 'test-execution)
(require 'test-parsing)
(require 'test-reporting)
(require 'test-retrieval)

;;
;; Functions to enable unit testing an org file
;; TODO: put parse-tests and run-tests in variables,
;; and use a macro to expand them, allowing the two to be defined together
;; simplifying the addition of new tests
;;
(cl-defstruct org-unit-test-extracts start end heading link text)
(cl-defstruct org-unit-test-group name tests link bound start)
(cl-defstruct org-unit-test type locator value)
;; Current test types:
'(:section-check :length-check :order-check :citation-check :mention-check :codeblock-check :tag-check)
(cl-defstruct org-unit-test-results name results link)

(defvar org-unit-test-heading-regexp "^*+ %s$"
  "Regexp to format when searching for a location")

(defvar org-unit-test-src-block-regexp "^ +#\\+begin_src"
  "Regexp to find src blocks")

(defun org-unit-test-to-string (test)
  (let ((tt (org-unit-test-type test))
        (loc (org-unit-test-locator test))
        (val (org-unit-test-value test))
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

(defun org-unit-test-org-file ()
  (interactive)
  ;; check file is in org mode
  (cl-assert (eq major-mode 'org-mode))
  ;; get tests
  (let* ((test-texts (seq-filter 'identity (org-unit-test-find-tests)))
         (parsed-tests (seq-filter 'identity (-flatten (mapcar 'org-unit-test-parse-tests test-texts))))
         (test-results (org-unit-test-run-tests parsed-tests))
         )
    ;; print test results
    (org-unit-test-print-results test-results)
    )
  )

(defvar org-unit-test-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode org-unit-test-minor-mode
  "  "
  :lighter "org-unit-test"
  :keymap org-unit-test-map
  )

;; TODO piggyback on org-export's tree walking
