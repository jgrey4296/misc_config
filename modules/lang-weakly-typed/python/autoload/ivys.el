;;; +exception_ivy.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +jg-python-ivy-exceptions '("BaseException" "SystemExit" "KeyboardInterrupt" "GeneratorExit" "Exception" "StopIteration" "StopAsyncIteration" "ArithmeticError" "FloatingPointError" "OverflowError" "ZeroDivisionError" "AssertionError" "AttributeError" "BufferError" "EOFError" "ImportError" "ModuleNotFoundError" "LookupError" "IndexError" "KeyError" "MemoryError" "NameError" "UnboundLocalError" "OSError" "BlockingIOError" "ChildProcessError" "ConnectionError" "BrokenPipeError" "ConnectionAbortedError" "ConnectionRefusedError" "ConnectionResetError" "FileExistsError" "FileNotFoundError" "InterruptedError" "IsADirectoryError" "NotADirectoryError" "PermissionError" "ProcessLookupError" "TimeoutError" "ReferenceError" "RuntimeError" "NotImplementedError" "RecursionError" "SyntaxError" "IndentationError" "TabError" "SystemError" "TypeError" "ValueError" "UnicodeError" "UnicodeDecodeError" "UnicodeEncodeError" "UnicodeTranslateError" "Warning" "DeprecationWarning" "PendingDeprecationWarning" "RuntimeWarning" "SyntaxWarning" "UserWarning" "FutureWarning" "ImportWarning" "UnicodeWarning" "BytesWarning" "ResourceWarning"))
;;;###autoload
(defvar +jg-python-ivy-datetimes '(
                                   "%a    Weekday as locale’s abbreviated name. Sun, Mon, …, Sat (en_US);"
                                   "%A    Weekday as locale’s full name. Sunday, Monday, …, Saturday (en_US);"
                                   "%w    Weekday as a decimal number, where 0 is Sunday and 6 is Saturday. 0, 1, …, 6"
                                   "%d    Day of the month as a zero-padded decimal number. 01, 02, …, 31"
                                   "%b    Month as locale’s abbreviated name.	Jan, Feb, …, Dec (en_US);"
                                   "%B    Month as locale’s full name.	January, February, …, December (en_US);"
                                   "%m    Month as a zero-padded decimal number.	01, 02, …, 12"
                                   "%y    Year without century as a zero-padded decimal number.	00, 01, …, 99"
                                   "%Y    Year with century as a decimal number.	0001, 0002, …, 2013, 2014, …, 9998, 9999"
                                   "%H    Hour (24-hour clock) as a zero-padded decimal number.	00, 01, …, 23"
                                   "%I    Hour (12-hour clock) as a zero-padded decimal number.	01, 02, …, 12"
                                   "%p    Locale’s equivalent of either AM or PM.	AM, PM (en_US);"
                                   "%M    Minute as a zero-padded decimal number.	00, 01, …, 59"
                                   "%S    Second as a zero-padded decimal number.	00, 01, …, 59"
                                   "%f    Microsecond as a decimal number, zero-padded to 6 digits.	000000, 000001, …, 999999"
                                   "%z    UTC offset in the form ±HHMM[SS[.ffffff]] (empty string if the object is naive).	(empty), +0000, -0400, +1030, +063415, -030712.345216"
                                   "%Z    Time zone name (empty string if the object is naive).	(empty), UTC, GMT"
                                   "%j    Day of the year as a zero-padded decimal number.	001, 002, …, 366"
                                   "%U    Week number of the year (Sunday as the first day of the week) as a zero-padded decimal number. All days in a new year preceding the first Sunday are considered to be in week 0.	00, 01, …, 53"
                                   "%W    Week number of the year (Monday as the first day of the week) as a zero-padded decimal number. All days in a new year preceding the first Monday are considered to be in week 0.	00, 01, …, 53"
                                   "%c    Locale’s appropriate date and time representation.	Tue Aug 16 21:30:00 1988 (en_US);"
                                   "%x    Locale’s appropriate date representation.	08/16/88 (None); 08/16/1988 (en_US);"
                                   "%X    Locale’s appropriate time representation.	21:30:00 (en_US);"
                                   "%%    A literal '%' character.	%"
                                   "%G    ISO 8601 year with century representing the year that contains the greater part of the ISO week (%V).	0001, 0002, …, 2013, 2014, …, 9998, 9999"
                                   "%u    ISO 8601 weekday as a decimal number where 1 is Monday.	1, 2, …, 7"
                                   "%V    ISO 8601 week as a decimal number with Monday as the first day of the week. Week 01 is the week containing Jan 4.	01, 02, …, 53"
                                   "https://docs.python.org/3/library/datetime.html"
                                   )
  )
;;;###autoload
(defvar +jg-python-ivy-argparse '(

                                  )
  )
;;;###autoload
(defvar +jg-python-ivy-libs-file (expand-file-name "~/.temp/pylibs"))
;;;###autoload
(defvar +jg-python-ivy-libs nil)

;;;###autoload
(defun +jg-python-exception-ivy ()
  (interactive)
  (insert (ivy-read "Exception Class: " +jg-python-ivy-exceptions :require-match t))
  )

;;;###autoload
(defun +jg-python-datetime-ivy ()
  (interactive)
  (insert (car (s-split " " (ivy-read "Datetime strptime: " +jg-python-ivy-datetimes :require-match t) t)))
  )

;;;###autoload
(defun +jg-python-libs-ivy ()
  (interactive)
  (unless +jg-python-ivy-libs
    (with-temp-buffer
      (insert-file-contents +jg-python-ivy-libs-file)
      (setq +jg-python-ivy-libs (s-split "\n" (buffer-string) t))
      )
    )
  (insert (car (s-split " " (ivy-read "Datetime strptime: " +jg-python-ivy-libs :require-match t) t)))
  )
