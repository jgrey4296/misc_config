;; Tracery-like text generation in lisp

;; Grammars of rulename : expansion_options
;; Where expansions in strings are wrapped in #'s.
;; Silent expansions at the head are separated by ||

(cl-defmacro push-grammar ()
  '(push (make-hash-table) grammar-stack)
  )
(cl-defmacro pop-grammar ()
  '(pop grammar-stack)
  )

(cl-defun tracerel-expand-pending (grammar-stack curr pending)
  ;; given the grammar stack, the current string,
  ;; and all the pending rule expansions,
  ;; expand them and return the updated string
  ;; (message "Expanding pending for: %s" curr)
  (let ((the-string curr))
    (cl-loop for match in pending
             do (let ((exp (tracerel-select-expansion grammar-stack (intern (nth 2 match)))))
                  (if exp
                      (setq the-string (s-replace (format "#%s#" (nth 2 match)) exp the-string))
                    )
                  )
             )
    the-string
    )
  )
(cl-defun tracerel-get-pending (str)
  ;;given a string, return the match data for all the next expansions
  "#\\([[:alpha:]]+\\)#"
  ;; (message "Getting Pending for: %s" str)
  (let ((results '())
        (last-match 0)
        )
    (while (string-match "#\\([_[:alpha:]]+\\)#" str last-match)
      (push `(,(match-beginning 0) ,(- (match-end 0) 1) ,(match-string 1 str)) results)
      (setq last-match (match-end 0))
      )
    ;; (message "Pending: %s" results)
    results
    )
  )
(cl-defun tracerel-select-expansion (grammar-stack rule)
  ;;Given a stack of grammars and a rule, go down it and expand based
  ;;based on the first success
  ;; (message "Selecting expansion for %s" rule)
  (let ((count 0)
        (stack-size (length grammar-stack))
        found-rule
        )
    (while (and (< count stack-size) (null found-rule))
      (setq found-rule (gethash rule (nth count grammar-stack)))
      (incf count)
      )
    (nth (random (length found-rule)) found-rule)
    )
  )
(defun tracerel-all-keys (grammar-stack)
  ;; (message "Getting all Keys")
  (let ((totality (make-hash-table :test 'equal))
        (count 0)
        (stack-size (length grammar-stack)))
    (while (< count stack-size)
      (mapc (lambda (x) (puthash (symbol-name x) t totality)) (hash-table-keys (nth count grammar-stack)))
      (incf count)
      )
    totality
    )
  )
(cl-defun tracerel-expand (grammar-stack rule &optional (curr-depth 0) (halt-depth nil))
  ;;Given a ctx::hashtable, grammar::hashtable and rule::string,
  ;;expand everything in it until
  ;;haltdepth is reached or all text are leaves
  ;; (message "Expanding for %s" rule)
  (let ((curr (tracerel-select-expansion grammar-stack rule))
        (all-keys (tracerel-all-keys grammar-stack))
        pending)
    (while (and (or (null halt-depth) (< curr-depth halt-depth))
                (setq pending (tracerel-get-pending curr))
                (some (lambda (x) (gethash (nth 2 x) all-keys)) pending))
      (setq curr (tracerel-expand-pending grammar-stack curr pending))
      (incf curr-depth)
      )
    curr
    )
  )
(cl-defun tracerel-run (grammar-stack &key (startpoint "start") (times 1) (dist nil) (haltdepth nil))
  ;; grammar as a hashtable
  ;; find startpoint
  ;; For times, expand the rule out, return as a list
  (mapcar (lambda (x) (tracerel-expand grammar-stack
                                       (intern startpoint)
                                       0
                                       haltdepth
                                       )) (make-list-as-big-as-n '(t) times))
  )
(cl-defmacro tracerel-grammar (&rest grammar)
  ;;Macro to transform tracery DSL into a hashtable
  (let* ((hash-grammar (make-hash-table))
         (remaining-grammar grammar)
         head last-symbol
         )
    (while (not (null remaining-grammar))
      (setq head (pop remaining-grammar))
      (cond ((stringp head) (push head (gethash last-symbol hash-grammar)))
            ((symbolp head) (progn (puthash head '() hash-grammar) (setq last-symbol head)))
            (t (message "Unrecognized grammar component")))
      )
    hash-grammar
    )
  )

;; Example

;; (let ((a (tracerel-grammar
;;           start "#beginning# #middle# #end#"
;;           beginning "There once was a boy named #name#." "On a dark and #weather# night"
;;           middle "There was a horrible murder." "The cruel #name# decided to eat some #food#."
;;           end "No one ever heard of them again"
;;           name "bob" "bill" "jill"
;;           weather "stormy" "snowy" "rainy" "cloudy" "misty"
;; )))
;;   (message "%s" (tracerel-run (list a)))
;;   )
