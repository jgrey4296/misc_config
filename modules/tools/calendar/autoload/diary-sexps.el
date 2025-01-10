;;; diary-sexps.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defconst jg-diary-dayname-plist '(:sun 0 :mon 1 :tue 2 :wed 3 :thu 4 :fri 5 :sat 6))

;; TODO: dsl parsing
;; TODO: macros?

;;;###autoload
(defun jg-diary-day-p (name)
  "Test for date to be the named day kwd"
  (let ((day (calendar-day-of-week date))
        (target (plist-get jg-diary-dayname-plist name))
        )
    (eq day target)
    )
  )

;;;###autoload
(defun jg-diary-count (start)
  "Get the number of days from start to date"
  (let ((date1 (calendar-absolute-from-gregorian
                (apply #'diary-make-date start)))
        (d (calendar-absolute-from-gregorian date))
        )
    (abs (- d date1))
    )
  )

;;;###autoload
(defun jg-diary-range (start end test)
  "Test to add diary entries within a range"
  (let ((date1 (calendar-absolute-from-gregorian
                (apply #'diary-make-date start)))
        (date2 (calendar-absolute-from-gregorian
                (apply #'diary-make-date end)))
        (d (calendar-absolute-from-gregorian date))
        )
    (message "Test was: %s" test)
    (and (<= date1 d)
         (< d date2)
         test
         )
    )
  )

;;;###autoload
(defun jg-diary-for (start length test)
  "Repeat an event from start for, if it passes test
length is same format as date
"
  (let ((date1 (calendar-absolute-from-gregorian
                (apply #'diary-make-date start)))
        (date2 (calendar-absolute-from-gregorian
                (apply #'diary-make-date
                       (mapcar #'(lambda (x) (+ (car x) (cdr x)))
                               (-zip start length))
                       )))
        (d (calendar-absolute-from-gregorian date))
        )
    (message "Test was: %s" test)
    (and (<= date1 d)
         (< d date2)
         test
         )
    )
  )

;;;###autoload
(defun jg-diary-tick-tock (n start test)
  "An entry that only occurs evey n ticks from a start date
(ticks are in days elapsed)
"
  (let ((elapsed (jg-diary-count start)))
    (and (eq 0 (% elapsed n))
         test
         )
    )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 10, 2025
;; Modified:   January 10, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; diary-sexps.el ends here
