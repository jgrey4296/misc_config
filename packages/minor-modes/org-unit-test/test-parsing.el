; Test Parsing

(defun org-unit-test-parse-tests (data)
  ;; split text into tests by full stops, defined groups,
  ;; and their originating heading
  (cl-assert (org-unit-test-extracts-p data))
  (if (not (string-empty-p (string-trim (org-unit-test-extracts-text data))))
      (let ((testgroups (parsec-with-input (org-unit-test-extracts-text data)
                          (parsec-many-till (org-unit-test-parse-group) (parsec-eof))))
            (link (org-unit-test-extracts-link data))
            (bound (org-unit-test-extracts-end data))
            (start (org-unit-test-extracts-start data))
            )
        (mapc (lambda (x)
                (cl-assert (org-unit-test-group-p x) t)
                (setf (org-unit-test-group-link x) link
                      (org-unit-test-group-bound x) bound
                      (org-unit-test-group-start x) start)) testgroups)
        testgroups
        )
    nil)
  )

(defun org-unit-test-parse-space ()
  (parsec-optional* (parsec-re "[[:blank:]]*")))

(defun org-unit-test-parse-word ()
  (parsec-return (parsec-re "[[:alnum:]]+")
    (org-unit-test-parse-space))
  )

(defun org-unit-test-parse-sword (word &optional retval)
  (let ((val (parsec-return (parsec-string word)
               (org-unit-test-parse-space))))
    (if retval retval val))
  )

(defun org-unit-test-parse-quote-string ()
  (mapconcat 'identity (parsec-between (parsec-ch ?\")
                                       (parsec-ch ?\")
                                       (parsec-many1 (org-unit-test-parse-word)))
             " ")
  )

(defun org-unit-test-parse-group-name ()
  (if (parsec-peek-p (parsec-re "[^:]+:\n"))
      (mapconcat 'identity
                 (parsec-many-till (org-unit-test-parse-word) (parsec-re ":"))
                 " ")
    "Anon Group")
  )

(defun org-unit-test-parse-group ()
  (org-unit-test-parse-space)
  (let* ((groupname (org-unit-test-parse-group-name))
         (tests (parsec-many-till (org-unit-test-parse-test) (parsec-or (parsec-eof) (parsec-re "\n\n"))))
        )
    (make-jg-org-test-group :name groupname :tests tests)
    )
  )

(defun org-unit-test-parse-test ()
  (parsec-re "\n?")
  (org-unit-test-parse-space)
  (let ((locator (parsec-or (org-unit-test-parse-quote-string)
                            (org-unit-test-parse-sword "Document" :document)
                            (org-unit-test-parse-sword "A section" :section)))
        keyword test
        )
    (org-unit-test-parse-space)
    (org-unit-test-parse-sword "should")
    (setq keyword (org-unit-test-parse-word)
          test (cond
                ((equal "have" keyword) (org-unit-test-parse-ownership-test))
                ((equal "be" keyword)   (org-unit-test-parse-existence-test))
                (t                      (org-unit-test-parse-misc-test keyword) )
                ))
    (setf (org-unit-test-locator test) locator)
    test))

(defun org-unit-test-parse-ownership-test ()
  ;; section/tag/codeblock
  (parsec-or (org-unit-test-parse-section-check)
             (org-unit-test-parse-tag-check)
             (org-unit-test-parse-codeblock-check)
             )
  )

(defun org-unit-test-parse-existence-test ()
  ;; length
  (org-unit-test-parse-length-check)
  )

(defun org-unit-test-parse-misc-test (keyword)
  ;; precede/cite/mention
  (cond
   ((equal keyword "precede") (org-unit-test-parse-order-check))
   ((equal keyword "cite") (org-unit-test-parse-citation-check))
   ((equal keyword "mention") (org-unit-test-parse-mention-check))
   )
  )

(defun org-unit-test-parse-section-check()
  (let ((test-type :section-check) subsection)
    (org-unit-test-parse-sword "section")
    (setq subsection (org-unit-test-parse-quote-string))
    (parsec-ch ?.)
    (make-jg-org-test :type test-type :value (downcase subsection))
    )
  )

(defun org-unit-test-parse-length-check ()
  (let ((test-type :length-check) dir length counter)
    (setq dir (parsec-or (org-unit-test-parse-sword "larger" :larger) (org-unit-test-parse-sword "smaller" :smaller)))
    (org-unit-test-parse-sword "than")
    (setq length (string-to-number (parsec-return (parsec-re "[[:digit:]]+") (org-unit-test-parse-space)))
          counter (parsec-or (org-unit-test-parse-sword "words" :words) (org-unit-test-parse-sword "paragraphs" :paras) (org-unit-test-parse-sword "sections" :sects))
          )
    (parsec-ch ?.)
    (make-jg-org-test :type test-type :value `(,dir ,length ,counter))
    )
  )

(defun org-unit-test-parse-order-check ()
  ;; locator should precede heading
  (let ((test-type :order-check) second)
    (setq second (org-unit-test-parse-quote-string))
    (parsec-ch ?.)
    (make-jg-org-test :type test-type :value second)
    )
  )

(defun org-unit-test-parse-citation-check ()
  ;; locator should cite citelist
  (let ((test-type :citation-check) citations)
    (parsec-ch ?:)
    (setq citations (parsec-sepby (parsec-re "[[:alnum:]]+") (parsec-ch ?,)))
    (parsec-ch ?.)
    (make-jg-org-test :type test-type :value citations)
    )
  )

(defun org-unit-test-parse-mention-check ()
  ;; locator should mention string
  (let ((test-type :mention-check) mention)
    (setq mention (org-unit-test-parse-quote-string))
    (parsec-ch ?.)
    (make-jg-org-test :type test-type :locator locator :value mention)
    )
  )

(defun org-unit-test-parse-codeblock-check ()
  ;; locator should have a codeblock
  (let ((test-type :codeblock-check) language)
    (org-unit-test-parse-sword "codeblock")
    (setq language (parsec-optional (parsec-and (org-unit-test-parse-sword "in") (org-unit-test-parse-quote-string))))
    (parsec-ch ?.)
    (make-jg-org-test :type test-type :value language)
    )
  )

(defun org-unit-test-parse-tag-check ()
  ;; locator should have tag x
  (let ((test-type :tag-check) mention)
    (org-unit-test-parse-sword "tag")
    (setq mention (org-unit-test-parse-quote-string))
    (parsec-ch ?.)
    (make-jg-org-test :type test-type :locator locator :value mention)
    )
  )
