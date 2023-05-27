;;; macros.el -*- lexical-binding: t; -*-


(cl-defmacro jgtest (val &body rest &key (test 2) &allow-other-keys)
  (while (keywordp (car rest)) (pop rest) (pop rest))
  `(got (val ,val)
    (:test ,test)
    (rest ,rest)
    )
  )

(jgtest val :test 5 blah blwee)
