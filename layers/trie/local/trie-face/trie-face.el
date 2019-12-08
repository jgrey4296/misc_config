;; Macros for generating faces programmatically for trie mode
(require 'dash)

(defgroup trie-face-generator '() "Variables of the macro based face generation")
(defcustom trie-defined-depth-count 10 "Number of Trie Faces to Generate" :type '(integer))
(defcustom trie-outermost-depth-face-count 3 "Generated Trie Faces depth" :type '(integer))
(defcustom trie-depth-face-name "trie-depth-" "Generated Trie Face Name Base" :type '(string))
(defcustom trie-depth-color-list '("color-26" "color-47" "color-99" "color-124"
                                   "color-129" "color-142" "color-164")
  "Default Generate Trie Face Colours" :type '(sexp))


(defun trie-face/flatten (lst)
  (letrec ((internal (lambda (x)
                       (cond
                        ((null x) nil)
                        ((atom x) (list x))
                        (t
                         (append (funcall internal (car x)) (funcall internal (cdr x))))))))
   (progn
      (assert (listp lst))
      (funcall internal lst))))

(defun trie-face/make-list-as-big-as-n (lst n)
  " Repeat a list up to size n"
  (progn
    (assert (listp lst))
    (assert (numberp n))
    (letrec ((lenlst (length lst)))
      (if (>= lenlst n)
          lst
        (letrec ((repN (+ 1 (/ n lenlst)))
                 (newLst (trie-face/flatten (-repeat repN lst))))
          (assert (>= (length newLst) n))
          newLst)))))

(defun trie-face/findFace (name)
  (seq-find (lambda (x) (string= (face-name x) name)) (face-list)))

;;adapted from rainbow-blocks.el
;;returns a string of a face name
(defun trie-face/trie-depth-face (depth)
  (let ((name (concat trie-depth-face-name
                      (number-to-string
                       (or
                        (and (<= depth trie-defined-depth-count)
                             depth)
                        ;;otherwise cycle
                        (+ 1 trie-outermost-depth-face-count
                           (mod (- depth trie-defined-depth-count 1)
                                (- trie-defined-depth-count
                                   trie-outermost-depth-face-count))))))))
    name))

(defmacro* trie-face/trie-generate-face (name color)
  `(defface ,name
       (list (list t :foreground ,color)) ;; :background "black"))
     "A Generated Face"
     :group 'trie-mode))

;;Create depth faces:
(defun trie-face/trie-face-creation ()
  (letrec (
           ;;create the range
           (range (number-sequence 0 (- trie-defined-depth-count 1)))
           ;;create the names
           (names (map 'sequence 'trie-face/trie-depth-face range))
           ;;Setup the color list at correct list length
           (colorList (trie-face/make-list-as-big-as-n trie-depth-color-list
                                             trie-defined-depth-count))
           ;;pair with a foreground color
           (paired (-zip-pair names colorList)))
    ;;now create the faces
    (dolist (pair paired)
      (let ((newFaceName (car pair))
            (newFaceColor (cdr pair)))
        (progn
          ;; (message "Creating Face: %s" newFaceName)
          ;; (message (format "%S" (macroexpand `(trie-generate-face ,(intern newFaceName)
          ;;                                                       newFaceColor))))
          (eval (macroexpand `(trie-face/trie-generate-face ,(intern newFaceName) newFaceColor))))))))

(trie-face/trie-face-creation)

(provide 'trie-face)
