;;
;; Functions to enable unit testing an org file
;; TODO: put parse-tests and run-tests in variables,
;; and use a macro to expand them, allowing the two to be defined together
;; simplifying the addition of new tests
;;
(require 'parsec)
(defstruct doctest/test-extracts start end heading link text)
(defstruct doctest/test-group name tests link bound start)
(defstruct doctest/test type locator value)
;; Current test types:
'(:section-check :length-check :order-check :citation-check :string-check :codeblock-check :tag-check)
(defstruct doctest/test-results name results link)

;; Test retrieval
(defun doctest/find-tests ()
  ;; iterate through current org file,
  ;; extract any __doctest__ drawers contents
  (save-excursion
    (goto-char (point-min))
    (org-map-entries 'doctest/extract-tests-from-entry)
    )
  )
(defun doctest/extract-tests-from-entry ()
  (let ((start (point)) end tests heading element link)
    (setq heading (substring-no-properties (org-get-heading t t t t))
          link (org-store-link nil)
          end (plist-get :contents-end (cadr (org-element-at-point))))
    (if (and (re-search-forward org-drawer-regexp end t)
             (s-equals? "__doctest__" (match-string 1))
             (s-equals? heading (org-get-heading t t t t)))
        (progn (setq element (cadr (org-element-at-point))
                     text (buffer-substring-no-properties (plist-get element :contents-begin)
                                                          (plist-get element :contents-end)))
               (make-test-extracts :start start :end end :heading heading :link link :test text)
               )
      nil
      )))
;; Test Parsing
(defun doctest/parse-tests (data)
  ;; split text into tests by full stops, defined groups,
  ;; and their originating heading
  (let ((testgroups ((parsec-with-input (doctest/test-extracts-text data)
                       (parsec-many1 (doctest/parse-group)))))
        (link (doctest/test-extracts-link data))
        (bound (doctest/test-extracts-end data))
        (start (doctest/test-extracts-start data))
        )
    (mapc (lambda (x) (setf (doctest/test-group-link x) link
                            (doctest/test-group-bound x) bound
                            (doctest/test-group-start x) start)) testgroups)
    testgroups
    )
  )

(defun doctest/parse-space ()
  (parsec-optional* (parsec-re "[[:space:]]*")))
(defun doctest/parse-word ()
  (doctest/parse-space)
  (parsec-return (parsec-re "[[:word:]]+")
    (doctest/parse-space))
  )
(defun doctest/parse-sword (word)
  (doctest/parse-space)
  (parsec-return (parsec-string word)
    (doctest/parse-space))
  )
(defun doctest/parse-quote-string ()
  (doctest/parse-space)
  (mapconcat 'identity (parsec-between (parsec-ch ?\")
                                       (parsec-ch ?\")
                                       (parsec-many1 (doctest/parse-word)))
             " ")
  )
(defun doctest/parse-group ()
  (let ((groupname (mapconcat 'identity (parsec-return (parsec-many1 (doctest/parse-word))
                                          (parsec-re ":\n"))) " ")
        (tests (parsec-many1 (doctest/parse-test)))
        )
    (parsec-re "^[[:space:]]*\n")
    (make-doctest/test-group :name groupname :tests tests)
    )
  )
(defun doctest/parse-test ()
  (parsec-try (doctest/parse-test-section-check)
              (doctest/parse-test-length)
              (doctest/parse-test-order)
              (doctest/parse-test-citation)
              (doctest/parse-test-string)
              (doctest/parse-test-codeblock)
              (doctest/parse-test-tag)
              )
  )

(defun doctest/parse-test-section-check ()
  (let ((test-type :section-check) container subsection)
    (setq container (parsec-or (parsec-string "Document") (doctester/parse-quote-string)))
    (parsec-string "should have section")
    (setq subsection (doctest/parse-quote-string))
    (make-doctest/test :type test-type :locator (downcase container) :value (downcase subsection))
    )
  )
(defun doctest/parse-test-length ()
  (let ((test-type :length-check) container dir length counter)
    (setq container (parsec-or (parsec-string "Document") (doctester/parse-quote-string)))
    (parsec-string " should be ")
    (setq dir (parsec-or (doctest/parse-sword "larger") (doctest/parse-sword "smaller")))
    (doctest/parse-sword "than")
    (setq length (string-to-number (parsec-re "[[:digit:]]+"))
          counter (parsec-or (parsec-str "words") (parsec-str "paragraphs") (parsec-str "sections"))
          )
    (make-doctest/test :type test-type :locator container :value `(,dir ,length ,counter))
    )
  )
(defun doctest/parse-test-order ()
  ;; locator should precede heading
  )
(defun doctest/parse-test-citation ()
  ;; locator should cite citelist
  )
(defun doctest/parse-test-string ()
  ;; locator should mention string
  )
(defun doctest/parse-test-codeblock ()
  ;; locator should have a codeblock
  )
(defun doctest/parse-test-tag ()
  ;; locator should have tag x
  )

;; Test Execution
(defun doctest/run-tests (testgroups)
  (mapcar 'doctest/run-test-group testgroups)
  )
(defun doctest/run-test-group (testgroup)
  (goto-char (doctest/test-group-start testgroup))
  (let ((name (doctest/test-group-name testgroup))
        (bound (doctest/test-group-bound testgroup))
        (results (mapcar (lambda (x) '("test-name" . ,(doctest/run-test x bound))) (doctest/test-group-tests testgroup)))
        )
    (make-doctest/test-results name results (doctest/test-group-link testgroup)))
  )
(defun doctest/run-test (test bound)
  (save-excursion
    (condition-case e
        (let ((type (doctest/test-type test)))
          (cond
           ((eq type :section-check )   (doctest/run-test-section-check test bound))
           ((eq type :length-check )    (doctest/run-test-length test bound))
           ((eq type :order-check )     (doctest/run-test-order test bound))
           ((eq type :citation-check )  (doctest/run-test-citation test bound))
           ((eq type :string-check )    (doctest/run-test-string test bound))
           ((eq type :codeblock-check ) (doctest/run-test-codeblock test bound))
           ((eq type :tag-check)        (doctest/run-test-tag test bound))
           (t (error "Unrecognized test type"))
           )
          )
      (search-failed nil)
      )
    )
  )

(defun doctest/run-test-section-check (test bound)
  ;;go to the locator
  (re-search-forward (format "^\*+ %s$" (doctest/test-locator test)) bound)
  ;; get subheadings
  (let ((subheadings (org-map-entries
                      (lambda () (downcase (substring-no-properties (org-get-heading)))) nil 'tree)))
    ;; is the section value in the subheadings?
    (-contains? subheadings (doctest/test-value))
    )
  )
(defun doctest/run-test-length (test bound)

  )
(defun doctest/run-test-order (test bound)

  )
(defun doctest/run-test-citation (test bound)

  )
(defun doctest/run-test-string (test bound)

  )
(defun doctest/run-test-codeblock (test bound)

  )
(defun doctest/run-test-tag (test bound)

  )

;; Test Reporting
(defun doctest/print-results (results)
  (with-temp-buffer-window "*Test Results*"
                           nil
                           nil
                           (insert "* Test Results\n")
                           (mapc 'doctest/print-test-results results)
                           (org-mode)
                           )
  )
(defun doctest/print-test-results(testgroup)
  (insert "** " (doctest/test-results-name testgroup) "\n")
  (insert "    " (doctest/test-results-link testgroup) "\n")
  (mapc (lambda (x) (insert "    " (if (cdr x) "✓" "X") " : " (car x) "\n")) (doctest/test-results-results testgroup))
  )

;; Main Access
(defun doctest/test-org-file ()
  (interactive)
  ;; check file is in org mode
  (assert (eq major-mode 'org-mode'))
  ;; get tests
  (let* ((test-texts (seq-filter 'identity (doctest/find-tests)))
         (parsed-tests (seq-filter 'identity (mapcar 'doctest/parse-tests test-texts)))
         (test-results (doctest/run-tests parsed-tests))
         )
    ;; print test results
    (doctest/print-results test-results)
    )
  )
