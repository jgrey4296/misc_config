;;; +exception_ivy.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar jg-python-insert-ivys (make-hash-table :test 'equal))

;;;###autoload
(defvar jg-python-ivy-exceptions '("BaseException" "SystemExit" "KeyboardInterrupt" "GeneratorExit" "Exception" "StopIteration" "StopAsyncIteration" "ArithmeticError" "FloatingPointError" "OverflowError" "ZeroDivisionError" "AssertionError" "AttributeError" "BufferError" "EOFError" "ImportError" "ModuleNotFoundError" "LookupError" "IndexError" "KeyError" "MemoryError" "NameError" "UnboundLocalError" "OSError" "BlockingIOError" "ChildProcessError" "ConnectionError" "BrokenPipeError" "ConnectionAbortedError" "ConnectionRefusedError" "ConnectionResetError" "FileExistsError" "FileNotFoundError" "InterruptedError" "IsADirectoryError" "NotADirectoryError" "PermissionError" "ProcessLookupError" "TimeoutError" "ReferenceError" "RuntimeError" "NotImplementedError" "RecursionError" "SyntaxError" "IndentationError" "TabError" "SystemError" "TypeError" "ValueError" "UnicodeError" "UnicodeDecodeError" "UnicodeEncodeError" "UnicodeTranslateError" "Warning" "DeprecationWarning" "PendingDeprecationWarning" "RuntimeWarning" "SyntaxWarning" "UserWarning" "FutureWarning" "ImportWarning" "UnicodeWarning" "BytesWarning" "ResourceWarning"))
;;;###autoload
(defvar jg-python-ivy-datetimes '(
                                   "%a   # Weekday as locale’s abbreviated name. Sun, Mon, …, Sat (en_US);"
                                   "%A   # Weekday as locale’s full name. Sunday, Monday, …, Saturday (en_US);"
                                   "%w   # Weekday as a decimal number, where 0 is Sunday and 6 is Saturday. 0, 1, …, 6"
                                   "%d   # Day of the month as a zero-padded decimal number. 01, 02, …, 31"
                                   "%b   # Month as locale’s abbreviated name.	Jan, Feb, …, Dec (en_US);"
                                   "%B   # Month as locale’s full name.	January, February, …, December (en_US);"
                                   "%m   # Month as a zero-padded decimal number.	01, 02, …, 12"
                                   "%y   # Year without century as a zero-padded decimal number.	00, 01, …, 99"
                                   "%Y   # Year with century as a decimal number.	0001, 0002, …, 2013, 2014, …, 9998, 9999"
                                   "%H   # Hour (24-hour clock) as a zero-padded decimal number.	00, 01, …, 23"
                                   "%I   # Hour (12-hour clock) as a zero-padded decimal number.	01, 02, …, 12"
                                   "%p   # Locale’s equivalent of either AM or PM.	AM, PM (en_US);"
                                   "%M   # Minute as a zero-padded decimal number.	00, 01, …, 59"
                                   "%S   # Second as a zero-padded decimal number.	00, 01, …, 59"
                                   "%f   # Microsecond as a decimal number, zero-padded to 6 digits.	000000, 000001, …, 999999"
                                   "%z   # UTC offset in the form ±HHMM[SS[.ffffff]] (empty string if the object is naive).	(empty), +0000, -0400, +1030, +063415, -030712.345216"
                                   "%Z   # Time zone name (empty string if the object is naive).	(empty), UTC, GMT"
                                   "%j   # Day of the year as a zero-padded decimal number.	001, 002, …, 366"
                                   "%U   # Week number of the year (Sunday as the first day of the week) as a zero-padded decimal number. All days in a new year preceding the first Sunday are considered to be in week 0.	00, 01, …, 53"
                                   "%W   # Week number of the year (Monday as the first day of the week) as a zero-padded decimal number. All days in a new year preceding the first Monday are considered to be in week 0.	00, 01, …, 53"
                                   "%c   # Locale’s appropriate date and time representation.	Tue Aug 16 21:30:00 1988 (en_US);"
                                   "%x   # Locale’s appropriate date representation.	08/16/88 (None); 08/16/1988 (en_US);"
                                   "%X   # Locale’s appropriate time representation.	21:30:00 (en_US);"
                                   "%%   # A literal '%' character.	%"
                                   "%G   # ISO 8601 year with century representing the year that contains the greater part of the ISO week (%V).	0001, 0002, …, 2013, 2014, …, 9998, 9999"
                                   "%u   # ISO 8601 weekday as a decimal number where 1 is Monday.	1, 2, …, 7"
                                   "%V   # ISO 8601 week as a decimal number with Monday as the first day of the week. Week 01 is the week containing Jan 4.	01, 02, …, 53"
                                   "https://docs.python.org/3/library/datetime.html"
                                   )
  )
;;;###autoload
(defvar jg-python-ivy-argparse '(

                                  )
  )
;;;###autoload
(defvar jg-python-ivy-libs-file (expand-file-name "~/.temp/pylibs"))
(defvar jg-python-ivy-libs-loaded nil)
;;;###autoload
(defvar jg-python-ivy-libs '("boltons.cacheutils       # caching            https://boltons.readthedocs.io/en/latest/cacheutils.html"
                             "boltons.debugutils       # debugging          https://boltons.readthedocs.io/en/latest/debugutils.html"
                             "boltons.dictutils        # mapping            https://boltons.readthedocs.io/en/latest/dictutils.html"
                             "boltons.ecoutils         # system analytics   https://boltons.readthedocs.io/en/latest/ecoutils.html"
                             "boltons.excutils         # exceptions         https://boltons.readthedocs.io/en/latest/excutils.html"
                             "boltons.fileutils        # filesystem         https://boltons.readthedocs.io/en/latest/fileutils.html"
                             "boltons.formatutils      # string formatting  https://boltons.readthedocs.io/en/latest/formatutils.html"
                             "boltons.funcutils        # functional         https://boltons.readthedocs.io/en/latest/funcutils.html"
                             "boltons.gcutils          # garbage collection https://boltons.readthedocs.io/en/latest/gcutils.html"
                             "boltons.ioutils          # files              https://boltons.readthedocs.io/en/latest/ioutils.html"
                             "boltons.iterutils        # iterators          https://boltons.readthedocs.io/en/latest/iterutils.html"
                             "boltons.jsonutils        # json               https://boltons.readthedocs.io/en/latest/jsonutils.html"
                             "boltons.listutils        # lists              https://boltons.readthedocs.io/en/latest/listutils.html"
                             "boltons.mathutils        # math               https://boltons.readthedocs.io/en/latest/mathutils.html"
                             "boltons.mboxutils        # mail               https://boltons.readthedocs.io/en/latest/mboxutils.html"
                             "boltons.namedutils       # tuples             https://boltons.readthedocs.io/en/latest/namedutils.html"
                             "boltons.pathutils        # path expansion     https://boltons.readthedocs.io/en/latest/pathutils.html"
                             "boltons.queueutils       # priority queues    https://boltons.readthedocs.io/en/latest/queueutils.html"
                             "boltons.setutils         # ordered sets       https://boltons.readthedocs.io/en/latest/setutils.html"
                             "boltons.socketutils      # sockets            https://boltons.readthedocs.io/en/latest/socketutils.html"
                             "boltons.statsutils       # statistics         https://boltons.readthedocs.io/en/latest/statsutils.html"
                             "boltons.strutils         # string formatting  https://boltons.readthedocs.io/en/latest/strutils.html"
                             "boltons.tableutils       # 2d data            https://boltons.readthedocs.io/en/latest/tableutils.html"
                             "boltons.tbutils          # tracebacks         https://boltons.readthedocs.io/en/latest/tbutils.html"
                             "boltons.timeutils        # datetimes          https://boltons.readthedocs.io/en/latest/timeutils.html"
                             "boltons.typeutils        # typing             https://boltons.readthedocs.io/en/latest/typeutils.html"
                             "boltons.urlutils         # urls               https://boltons.readthedocs.io/en/latest/urlutils.html"
                             "pyparsing as pp          # https://pyparsing-docs.readthedocs.io/en/latest/"
                             "bs4                      # https://beautiful-soup-4.readthedocs.io/en/latest/"
                             "matplotlib.pyplot as plt # https://matplotlib.org/stable/api/index.html"
                             "more_itertools as mitz   # https://more-itertools.readthedocs.io/en/stable/"
                             "networkx as nx           # https://networkx.org/"
                             "numpy as np              # https://numpy.org/doc/stable/"
                             "seaborn as sns           # https://seaborn.pydata.org/api.html"
                             "stackprinter             # https://github.com/cknd/stackprinter"
                             "toolz                    # https://toolz.readthedocs.io/en/latest/index.html"
                             "pytest                   # https://docs.pytest.org/en/7.3.x/reference/reference.html#std-fixture-pytestconfig"
                             ))

;;;###autoload
(defvar jg-python-ivy-pytest-fixtures '("caplog          # capture logging "
                                        "recwarn         # capture warnings"
                                        "capfd           # capture file descriptors 1 and 2"
                                        "capfdbinary     # capture file descriptors 1 and 2 binary"
                                        "capsys          # capture stdout, stderr "
                                        "pytestconfig    # access pytest config "
                                        "record_property # add key:value pairs to reports"
                                        "cache           # store values between runs"
                                        "tmp_path        # unique temp directory"
                                        "pytester        # for black box testing of plugins"
                                        "monkeypatch     # modify test context"
                                        "mocker          # create mock objects"
                                        "                # https://docs.pytest.org/en/7.3.x/reference/fixtures.html"
                                        "                # https://pytest-mock.readthedocs.io/en/latest/"
                                        ))


;;;###autoload
(defun +jg-python-exception-ivy ()
  (interactive)
  (insert "raise " (ivy-read "Exception Class: " jg-python-ivy-exceptions :require-match t))
  )
(puthash "raise" #'+jg-python-exception-ivy jg-python-insert-ivys)

;;;###autoload
(defun +jg-python-datetime-ivy ()
  (interactive)
  (insert (car (s-split " " (ivy-read "Datetime strptime: " jg-python-ivy-datetimes :require-match t) t)))
  )
(puthash "datetime" #'+jg-python-datetime-ivy jg-python-insert-ivys)

;;;###autoload
(defun +jg-python-imports-ivy ()
  (interactive)
  (unless jg-python-ivy-libs-loaded
    (with-temp-buffer
      (insert-file-contents jg-python-ivy-libs-file)
      (setq jg-python-ivy-libs (sort (append jg-python-ivy-libs
                                             (s-split "\n" (buffer-string) t)
                                             )
                                     #'string-lessp)
            jg-python-ivy-libs-loaded t)
      )
    )
  (insert "import " (car (s-split " " (ivy-read "Import Library: " jg-python-ivy-libs :require-match t) t)))
  )
(puthash "import" #'+jg-python-imports-ivy jg-python-insert-ivys)

;;;###autoload
(defun +jg-python-pytest-fixtures-ivy ()
  (insert (car (s-split " " (ivy-read "Datetime strptime: " jg-python-ivy-pytest-fixtures :require-match t) t)))
  )
(puthash "fixtures" #'+jg-python-pytest-fixtures-ivy jg-python-insert-ivys)
