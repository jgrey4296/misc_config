
;; Test
(:type x :locator y :value z :group g)


(parsec-with-input text
  (parsec-many1 (parse-text)))


(defun parse-text ()
  (parsec-optional (parse-group-header))
  (parsec-many1 (parse-test))
  (parsec-or (parse-empty-line) (parse-text-end)))

(defun parse-test ()
  (parsec-or (parsec-string "Document")
             (parse-string "A Section")
             (parse-section-name))
  (parsec-or (parse-test-condition))
  (parsec-ch ?.)
)
