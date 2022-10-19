;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Macros for generating faces programmatically for acab mode
(require 'dash)
(require 'cl-lib)

(defgroup acab-face-generator '() "Variables of the macro based face generation")
(defcustom acab-defined-depth-count 10 "Number of Acab Faces to Generate" :type '(integer))
(defcustom acab-outermost-depth-face-count 3 "Generated Acab Faces depth" :type '(integer))
(defcustom acab-depth-face-name "acab-depth-" "Generated Acab Face Name Base" :type '(string))
(defcustom acab-depth-color-list '("#87d1ef" "#64a1c2" "#466480" "#2f485c" "#242e35" "#1b2026" "#aa9c8a" "#917f6d" "#86624a")
  "Default Generate Acab Face Colours" :type '(sexp))


;;--------------------
;;face definitions
;; use them as symbols 'blah in font-lock-keywords
;;--------------------
(defface acab-rulename
  '((t
     :foreground "green"
     :background "black"
     :underline t))
  "Face for Rule names"
  :group 'acab-mode)
(defface acab-ruleend
  '((t
     :foreground "red"
     :background "black"
     :underline t
     ))
  "Face for the end of rules"
  :group 'acab-mode)
(defface acab-closure
  '((t
     :background "blue"
     ))
  "Face for Enclosed sections"
  :group 'acab-mode)


;; Functions
(defun acab-face/flatten (lst)
  (letrec ((internal (lambda (x)
                       (cond
                        ((null x) nil)
                        ((atom x) (list x))
                        (t
                         (append (funcall internal (car x)) (funcall internal (cdr x))))))))
   (progn
      (cl-assert (listp lst))
      (funcall internal lst))))
(defun acab-face/make-list-as-big-as-n (lst n)
  " Repeat a list up to size n"
  (progn
    (cl-assert (listp lst))
    (cl-assert (numberp n))
    (letrec ((lenlst (length lst)))
      (if (>= lenlst n)
          lst
        (letrec ((repN (+ 1 (/ n lenlst)))
                 (newLst (acab-face/flatten (-repeat repN lst))))
          (cl-assert (>= (length newLst) n))
          newLst)))))
(defun acab-face/findFace (name)
  (seq-find (lambda (x) (string= (face-name x) name)) (face-list)))

;;adapted from rainbow-blocks.el
;;returns a string of a face name
(defun acab-face/acab-depth-face (depth)
  (let ((name (concat acab-depth-face-name
                      (number-to-string
                       (or
                        (and (<= depth acab-defined-depth-count)
                             depth)
                        ;;otherwise cycle
                        (+ 1 acab-outermost-depth-face-count
                           (mod (- depth acab-defined-depth-count 1)
                                (- acab-defined-depth-count
                                   acab-outermost-depth-face-count))))))))
    name))

(defmacro* acab-face/acab-generate-face (name color)
  `(defface ,name
       (list (list t :foreground ,color)) ;; :background "black"))
     "A Generated Face"
     :group 'acab-mode))

;;Create depth faces:
(defun acab-face/acab-face-creation ()
  (letrec (
           ;;create the range
           (range (number-sequence 0 (- acab-defined-depth-count 1)))
           ;;create the names
           (names (map 'sequence 'acab-face/acab-depth-face range))
           ;;Setup the color list at correct list length
           (colorList (acab-face/make-list-as-big-as-n acab-depth-color-list
                                             acab-defined-depth-count))
           ;;pair with a foreground color
           (paired (-zip-pair names colorList)))
    ;;now create the faces
    (dolist (pair paired)
      (let ((newFaceName (car pair))
            (newFaceColor (cdr pair)))
        (progn
          ;; (message "Creating Face: %s" newFaceName)
          ;; (message (format "%S" (macroexpand `(acab-generate-face ,(intern newFaceName)
          ;;                                                       newFaceColor))))
          (eval (macroexpand `(acab-face/acab-generate-face ,(intern newFaceName) newFaceColor))))))))

(acab-face/acab-face-creation)

(provide 'acab-face)
