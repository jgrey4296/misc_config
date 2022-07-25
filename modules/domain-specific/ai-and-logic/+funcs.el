;;; +funcs.el -*- lexical-binding: t; -*-

;; TODO prolog rule manager

 ;; |                           |                                            |                                    |                                |             |
 ;; | constraints               | consumers                                  | focus: pred(X)                     | precursors                     | facts.      |
 ;; |---------------------------+--------------------------------------------+------------------------------------+--------------------------------+-------------|
 ;; |                           |                                            |                                    |                                |             |
 ;; | :- pred(X).               | other(X) :- pred(X).                       | pred(X) :- prior(X, Y), prior2(Y). | prior(X,Y) :- a, b, c.         | pred(bob).  |
 ;; | :- pred(X), not other(X). | other(X) :- pred(X), some.                 | pred(X) :- prior(X, Y), prior3(Y). | prior(X)   :- d, e, f.         | pred(bill). |
 ;; |                           |                                            | pred(X) :- prior(X).               |                                |             |
 ;; |                           | another :- pred(_).                        |                                    | prior2(Y)  :- something, else. |             |
 ;; |                           |                                            |                                    |                                |             |
 ;; |                           | different(X) :- some, diff(X, Y), pred(Y). |                                    | prior3(Y)  :- final.           |             |
 ;; |                           |                                            |                                    |                                |             |


;; Also: initial fact list, all heads list, constraints list
