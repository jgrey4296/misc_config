;;
;; Functions to enable unit testing an org file
;; TODO: put parse-tests and run-tests in variables,
;; and use a macro to expand them, allowing the two to be defined together
;; simplifying the addition of new tests
;;
(defstruct doctest/test-extracts start end heading link text)
(defstruct doctest/test-group name tests link bound start)
(defstruct doctest/test type locator value)
;; Current test types:
'(:section-check :length-check :order-check :citation-check :mention-check :codeblock-check :tag-check)
(defstruct doctest/test-results name results link)

(defvar doctest/heading-regexp "^*+ %s$"
  "Regexp to format when searching for a location")

(defvar doctest/src-block-regexp "^ +#\\+begin_src"
  "Regexp to find src blocks")

(defun doctest/test-to-string (test)
  (let ((tt (doctest/test-type test))
        (loc (doctest/test-locator test))
        (val (doctest/test-value test))
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
                     text (string-trim (buffer-substring-no-properties (plist-get element :contents-begin)
                                                                       (plist-get element :contents-end))))
               (if (not (string-empty-p text))
                   (make-doctest/test-extracts :start start :end end :heading heading :link link :text text)
                 nil)
               )
      nil
      )))
;; Test Parsing
(defun doctest/parse-tests (data)
  ;; split text into tests by full stops, defined groups,
  ;; and their originating heading
  (assert (doctest/test-extracts-p data))
  (message "Parsing Tests")
  (if (not (string-empty-p (string-trim (doctest/test-extracts-text data))))
      (let ((testgroups (parsec-with-input (doctest/test-extracts-text data)
                          (parsec-many-till (doctest/parse-group) (parsec-eof))))
            (link (doctest/test-extracts-link data))
            (bound (doctest/test-extracts-end data))
            (start (doctest/test-extracts-start data))
            )
        (mapc (lambda (x)
                (assert (doctest/test-group-p x) t)
                (setf (doctest/test-group-link x) link
                      (doctest/test-group-bound x) bound
                      (doctest/test-group-start x) start)) testgroups)
        testgroups
        )
    nil)
  )

(defun doctest/parse-space ()
  (parsec-optional* (parsec-re "[[:blank:]]*")))
(defun doctest/parse-word ()
  (parsec-return (parsec-re "[[:alnum:]]+")
    (doctest/parse-space))
  )
(defun doctest/parse-sword (word &optional retval)
  (let ((val (parsec-return (parsec-string word)
               (doctest/parse-space))))
    (if retval retval val))
  )
(defun doctest/parse-quote-string ()
  (mapconcat 'identity (parsec-between (parsec-ch ?\")
                                       (parsec-ch ?\")
                                       (parsec-many1 (doctest/parse-word)))
             " ")
  )

(defun doctest/parse-group ()
  ;;TODO: make groupname optional
  (message "Parsing group")
  (doctest/parse-space)
  (let ((groupname (mapconcat 'identity (parsec-many-till (doctest/parse-word) (parsec-re ":")) " "))
        (tests (parsec-many-till (doctest/parse-test)  (parsec-or (parsec-re "\n\n") (parsec-eof))))
        )
    (make-doctest/test-group :name groupname :tests tests)
    )
  )
(defun doctest/parse-test ()
  (message "Parsing test")
  (parsec-re "\n")
  (doctest/parse-space)
  (let ((locator (parsec-or (doctest/parse-quote-string)
                            (doctest/parse-sword "Document" :document)
                            (doctest/parse-sword "A section" :section)))
        keyword test
        )
    (doctest/parse-space)
    (doctest/parse-sword "should")
    (setq keyword (doctest/parse-word)
          test (cond
                ((equal "have" keyword) (doctest/parse-ownership-test))
                ((equal "be" keyword)   (doctest/parse-existence-test))
                (t                      (doctest/parse-misc-test keyword) )
                ))
    (setf (doctest/test-locator test) locator)
    test))

(defun doctest/parse-ownership-test ()
  ;; section/tag/codeblock
  (parsec-or (doctest/parse-section-check)
             (doctest/parse-tag-check)
             (doctest/parse-codeblock-check)
             )
  )
(defun doctest/parse-existence-test ()
  ;; length
  (doctest/parse-length-check)
  )
(defun doctest/parse-misc-test (keyword)
  ;; precede/cite/mention
  (cond
   ((equal keyword "precede") (doctest/parse-order-check))
   ((equal keyword "cite") (doctest/parse-citation-check))
   ((equal keyword "mention") (doctest/parse-mention-check))
   )
  )

(defun doctest/parse-section-check()
  (message "Parsing section check")
  (let ((test-type :section-check) subsection)
    (doctest/parse-sword "section")
    (setq subsection (doctest/parse-quote-string))
    (parsec-ch ?.)
    (make-doctest/test :type test-type :value (downcase subsection))
    )
  )
(defun doctest/parse-length-check ()
  (message "Parsing length check")
  (let ((test-type :length-check) dir length counter)
    (setq dir (parsec-or (doctest/parse-sword "larger" :larger) (doctest/parse-sword "smaller" :smaller)))
    (doctest/parse-sword "than")
    (setq length (string-to-number (parsec-return (parsec-re "[[:digit:]]+") (doctest/parse-space)))
          counter (parsec-or (doctest/parse-sword "words" :words) (doctest/parse-sword "paragraphs" :paras) (doctest/parse-sword "sections" :sects))
          )
    (parsec-ch ?.)
    (make-doctest/test :type test-type :value `(,dir ,length ,counter))
    )
  )
(defun doctest/parse-order-check ()
  ;; locator should precede heading
  (message "Parsing order check")
  (let ((test-type :order-check) second)
    (setq second (doctest/parse-quote-string))
    (parsec-ch ?.)
    (make-doctest/test :type test-type :value second)
    )
  )
(defun doctest/parse-citation-check ()
  ;; locator should cite citelist
  (message "Parsing citation Check")
  (let ((test-type :citation-check) citations)
    (parsec-ch ?:)
    (setq citations (parsec-sepby (parsec-re "[[:alnum:]]+") (parsec-ch ?,)))
    (parsec-ch ?.)
    (make-doctest/test :type test-type :value citations)
    )
  )
(defun doctest/parse-mention-check ()
  ;; locator should mention string
  (message "Parsing mention check")
  (let ((test-type :mention-check) mention)
    (setq mention (doctest/parse-quote-string))
    (parsec-ch ?.)
    (make-doctest/test :type test-type :locator locator :value mention)
    )
  )
(defun doctest/parse-codeblock-check ()
  ;; locator should have a codeblock
  (message "Parsing codeblock check")
  (let ((test-type :codeblock-check) language)
    (doctest/parse-sword "codeblock")
    (setq language (parsec-optional (parsec-and (doctest/parse-sword "in") (doctest/parse-quote-string))))
    (parsec-ch ?.)
    (make-doctest/test :type test-type :value language)
    )
  )
(defun doctest/parse-tag-check ()
  ;; locator should have tag x
  (message "Parsing tag check")
  (let ((test-type :tag-check) mention)
    (doctest/parse-sword "tag")
    (setq mention (doctest/parse-quote-string))
    (parsec-ch ?.)
    (make-doctest/test :type test-type :locator locator :value mention)
    )
  )

;; Test Execution
(defun doctest/forward-to (locator bound)
  (cond
   ((eq :document locator) nil)
   ((eq :section locator) nil)
   (t (re-search-forward (format doctest/heading-regexp locator) bound))
   )
  )
(defun doctest/forward-past-tests (locator bound)
  (doctest/forward-to locator bound)
  (let ((orig (point)))
    (re-search-forward org-drawer-regexp bound)
    (if (s-equals? "__doctest__" (match-string 1))
        (re-search-forward org-drawer-regexp bound)
      (goto-char orig))
    )
  )

(defun doctest/run-tests (testgroups)
  (message "Running Tests")
  (mapcar 'doctest/run-test-group testgroups)
  )
(defun doctest/run-test-group (testgroup)
  (assert (doctest/test-group-p testgroup) t)
  (message "Running test group")
  (goto-char (doctest/test-group-start testgroup))
  (let* ((name (doctest/test-group-name testgroup))
         (bound (doctest/test-group-bound testgroup))
         (results (mapcar (lambda (x) `(,(doctest/test-to-string x) . ,(doctest/run-test x bound))) (doctest/test-group-tests testgroup)))
         )
    (make-doctest/test-results :name name :results results :link (doctest/test-group-link testgroup)))
  )
(defun doctest/run-test (test bound)
  (assert (doctest/test-p test))
  (message "Running test")
  (save-excursion
    (condition-case e
        (let ((type (doctest/test-type test)))
          (cond
           ((eq type :section-check )   (doctest/run-test-section test bound))
           ((eq type :length-check )    (doctest/run-test-length test bound))
           ((eq type :order-check )     (doctest/run-test-order test bound))
           ((eq type :citation-check )  (doctest/run-test-citation test bound))
           ((eq type :mention-check )    (doctest/run-test-mention test bound))
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
