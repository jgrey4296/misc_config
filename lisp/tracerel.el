;; Tracery-like text generation in lisp

;; Grammars of rulename : expansion_options
;; Where expansions in strings are wrapped in #'s.
;; Silent expansions at the head are separated by ||

(require 'parsec)
;; Parsec functions to parse the dsl of #expansions.andMods#
(defun tracerel-parse-spaces ()
  (parsec-optional* (parsec-re "[[:space:]]*")))
(defun tracerel-parse-par ()
  (parsec-one-of ?\( ?\)))
(defun tracerel-parse-hash ()
  (parsec-str "#")
  )
(defun tracerel-parse-word ()
  (let (result)
    (tracerel-parse-spaces)
    (setq result (parsec-re "[[:alpha:]]+"))
    (tracerel-parse-spaces)
    result))
(defun tracerel-parse-modifier ()
  (parsec-str ".")
  (tracerel-parse-word)
  )
(defun tracerel-parse-rule()
  (let ((vals '()))
    (push (parsec-optional-maybe (tracerel-parse-word)) vals)
    (push :rule vals)
    (push (parsec-optional-maybe (tracerel-parse-modifier)) vals)
    (push :mod vals)
    vals
    ))
(defun tracerel-parsec(str)
  (parsec-with-input str
    (parsec-between (tracerel-parse-hash)
                    (tracerel-parse-hash)
                    (tracerel-parse-rule))
    )
  )

;; Utility functions
(cl-defun tracerel-add-to-grammar (k v)
  " TODO: modifier to add a key value pair to the top grammar of the stack "
  nil
)
(cl-defmacro tracerel-push-grammar ()
  " Push an empty grammar onto the grammar stack "
  '(push (make-hash-table) grammar-stack)
  )
(cl-defmacro tracerel-pop-grammar ()
  " Pop a grammar off the grammar stack "
  '(pop grammar-stack)
  )

;; Core Tracerel
(cl-defun tracerel-call-mod (exp mod funcs)
  " Call a modifier on an expansion "
  (if (gethash mod funcs)
      (funcall (gethash mod funcs) exp)
    exp)
  )
(cl-defun tracerel-expand-pending (grammar-stack curr pending &optional funcs)
  " Expand all currently pending expansions in a string "
  (let ((the-string curr))
    (cl-loop for match in pending
             do (let* ((rule (plist-get match :rule))
                       (mod (plist-get match :mod))
                       (exp (if (parsec-from-maybe rule)
                                (tracerel-select-expansion grammar-stack (intern (parsec-from-maybe rule)))
                              nil)))
                  (if (and exp funcs (parsec-from-maybe mod))
                      (setq exp (tracerel-call-mod exp (parsec-from-maybe mod) funcs))
                    )
                  (if exp
                      ;;Todo: can’t handle multiples
                      (setq the-string (s-replace (format "%s" (plist-get match :raw)) exp the-string))
                    )
                  )
             )
    the-string
    )
  )
(cl-defun tracerel-get-pending (str)
  " Get all pending rule expansions in a string, as a list of p-lists,
from in LIFO order "
  (let ((results '())
        (last-match 0)
        (curr '())
        )
    (while (string-match "#\\([_.[:alpha:]]+\\)#" str last-match)
      (setq last-match (match-end 0))

      (push (list :begin (match-beginning 0)) curr)
      (push (list :end (- (match-end 0) 1)) curr)
      (push (list :raw (match-string 0 str)) curr)
      (push (tracerel-parsec (match-string 0 str)) curr)
      ;;flatten into a p-list:
      (push (-flatten curr) results)
      (setq curr '())
      )
    ;; (message "Pending: %s" results)
    results
    )
  )
(cl-defun tracerel-select-expansion (grammar-stack rule)
  " Selects from multiple expansion options of a rule "
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
(cl-defun tracerel-all-keys (grammar-stack)
  " Gets all keys from the grammar stack to use for auto-exit of tracerel-run "
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
(cl-defun tracerel-expand (grammar-stack rule &optional (curr-depth 0) (halt-depth nil) (funcs nil))
  ;;Given a ctx::hashtable, grammar::hashtable and rule::string,
  ;;expand everything in it until
  ;;haltdepth is reached or all text are leaves
  ;; (message "Expanding for %s" rule)
  (let ((curr (tracerel-select-expansion grammar-stack rule))
        (all-keys (tracerel-all-keys grammar-stack))
        pending)
    (while (and (or (null halt-depth) (< curr-depth halt-depth))
                (setq pending (tracerel-get-pending curr))
                ;;Todo: to match all keys, needs to discard funcs
                (some (lambda (x) (if (parsec-from-maybe (plist-get x :rule))
                                      (gethash (parsec-from-maybe (plist-get x :rule)) all-keys))) pending))
      (setq curr (tracerel-expand-pending grammar-stack curr pending funcs))
      (incf curr-depth)
      )
    curr
    )
  )
(cl-defun tracerel-run (grammar-stack &key (startpoint "start") (times 1) (haltdepth nil) (funcs nil))
  " Runs Tracerel on a stack of grammars with a starting rule,
can run multiple times, to an arbitrary limit depth, and with a hashtable of transform functions "
  (mapcar (lambda (x) (tracerel-expand grammar-stack
                                       (intern startpoint)
                                       0
                                       haltdepth
                                       funcs
                                       )) (make-list-as-big-as-n '(t) times))
  )
(cl-defmacro tracerel-grammar (&rest grammar)
  " Macro to transform tracery DSL into a hashtable "
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
(defun test-tracerel ()
  (let ((a (tracerel-grammar
            start "#beginning# #middle# #end#"
            beginning "There once was a boy named #name.testfunc#." "On a dark and #weather# night,"
            middle "There was a horrible murder." "The cruel #name# decided to eat some #food#."
            end "No one ever heard of them again"
            name "bob" "bill" "jill"
            weather "stormy" "snowy" "rainy" "cloudy" "misty"
            ))
        (funcs (make-hash-table :test 'equal))
        )
    (puthash "testfunc" (lambda (x) (concat "[" x "]")) funcs)
    (message "%s" (tracerel-run (list a) :funcs funcs))
    )
)
