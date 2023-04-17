;;; +exception_ivy.el -*- lexical-binding: t; -*-

(defvar +jg-python-ivy-exceptions '("BaseException" "SystemExit" "KeyboardInterrupt" "GeneratorExit" "Exception" "StopIteration" "StopAsyncIteration" "ArithmeticError" "FloatingPointError" "OverflowError" "ZeroDivisionError" "AssertionError" "AttributeError" "BufferError" "EOFError" "ImportError" "ModuleNotFoundError" "LookupError" "IndexError" "KeyError" "MemoryError" "NameError" "UnboundLocalError" "OSError" "BlockingIOError" "ChildProcessError" "ConnectionError" "BrokenPipeError" "ConnectionAbortedError" "ConnectionRefusedError" "ConnectionResetError" "FileExistsError" "FileNotFoundError" "InterruptedError" "IsADirectoryError" "NotADirectoryError" "PermissionError" "ProcessLookupError" "TimeoutError" "ReferenceError" "RuntimeError" "NotImplementedError" "RecursionError" "SyntaxError" "IndentationError" "TabError" "SystemError" "TypeError" "ValueError" "UnicodeError" "UnicodeDecodeError" "UnicodeEncodeError" "UnicodeTranslateError" "Warning" "DeprecationWarning" "PendingDeprecationWarning" "RuntimeWarning" "SyntaxWarning" "UserWarning" "FutureWarning" "ImportWarning" "UnicodeWarning" "BytesWarning" "ResourceWarning"))
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
(defvar +jg-python-ivy-argparse '(

                                  )
  )
(defvar +jg-python-ivy-libs-file (expand-file-name "~/.temp/pylibs"))
(defvar +jg-python-ivy-libs '("alabaster" "applaunchservices" "appnope" "arrow" "astroid" "asttokens" "atomicwrites" "attrs" "automat" "autopep8" "babel" "backcall" "beautifulsoup4" "bibtexparser" "binaryornot" "black" "blas" "bleach" "blinker" "blurhash" "brotli" "brotli-bin" "brotlipy" "build" "bzip2" "ca-certificates" "certifi" "cffi" "cfgv" "chardet" "charset-normalizer" "citeproc-py" "click" "cloudpickle" "comm" "constantly" "cookiecutter" "cryptography" "cssselect" "cycler" "debugpy" "decorator" "defusedxml" "diff-match-patch" "dill" "distlib" "docopt" "docstring-to-markdown" "docutils" "doit" "doit-graph" "doot" "entrypoints" "executing" "fastjsonschema" "feedgenerator" "fftw" "filelock" "flake8" "fonttools" "freetype" "future" "giflib" "hyperlink" "icu" "identify" "idna" "imagesize" "importlib-metadata" "incremental" "inflection" "intel-openmp" "intervaltree" "ipykernel" "ipython" "ipython-genutils" "isort" "itemadapter" "itemloaders" "jaraco-classes" "jedi" "jellyfish" "jinja2" "jinja2-time" "jmespath" "joblib" "jpeg" "jsonschema" "jupyter-client" "jupyter-core" "jupyterlab-pygments" "keyring" "kiwisolver" "lazy-object-proxy" "lcms2" "lerc" "libbrotlicommon" "libbrotlidec" "libbrotlienc" "libcxx" "libdeflate" "libffi" "libgfortran" "libgfortran5" "libiconv" "libpng" "libtiff" "libwebp" "libwebp-base" "libxml2" "libxslt" "llvm-openmp" "lxml" "lz4-c" "markdown" "markdown-it-py" "markdown-word-count" "markupsafe" "mastodon-py" "matplotlib" "matplotlib-base" "matplotlib-inline" "mccabe" "mdurl" "mistune" "mkl" "mkl-service" "mkl_fft" "mkl_random" "more-itertools" "munkres" "mypy-extensions" "nbclient" "nbconvert" "nbformat" "ncurses" "nest-asyncio" "networkx" "nodeenv" "numpy" "numpy-base" "numpydoc" "oauthlib" "openssl" "packaging" "pandoc" "pandocfilters" "parsel" "parso" "pathspec" "pdfrw" "pelican" "pelican-graphviz" "pelican-image-process" "pelican-liquid-tags" "pelican-pandoc-reader" "pelican-seo" "pelican-sitemap" "pelican-thumbnailer" "pelican-webassets" "pep517" "pexpect" "pickleshare" "pillow" "pip" "pip-chill" "pipreqs" "platformdirs" "pluggy" "pony" "pre-commit" "prompt-toolkit" "protego" "psutil" "ptyprocess" "pure-eval" "pure-python-adb" "pyasn1" "pyasn1-modules" "pycodestyle" "pycparser" "pydispatcher" "pydocstyle" "pyflakes" "pygments" "pygraphviz" "pyjwt" "pylatexenc" "pylint" "pylint-venv" "pyls-spyder" "pymupdf" "pyobjc-core" "pyobjc-framework-cocoa" "pyobjc-framework-coreservices" "pyobjc-framework-fsevents" "pyopenssl" "pypandoc" "pyparsing" "pypdf2" "pyqt5" "pyqt5-qt5" "pyqt5-sip" "pyqtwebengine" "pyqtwebengine-qt5" "pyrsistent" "pysocks" "python" "python-dateutil" "python-liquid" "python-lsp-black" "python-lsp-jsonrpc" "python-lsp-server" "python-magic" "python-slugify" "python-twitter" "pytoolconfig" "pytz" "pyyaml" "pyzmq" "qdarkstyle" "qstylizer" "qtawesome" "qtconsole" "qtpy" "queuelib" "readline" "regex" "requests" "requests-file" "requests-oauthlib" "rich" "rope" "rsa" "rtree" "ruamel-yaml" "ruamel-yaml-clib" "scikit-learn" "scipy" "scrapy" "service-identity" "setuptools" "six" "snowballstemmer" "sortedcontainers" "soupsieve" "sphinx" "sphinxcontrib-applehelp" "sphinxcontrib-devhelp" "sphinxcontrib-htmlhelp" "sphinxcontrib-jsmath" "sphinxcontrib-qthelp" "sphinxcontrib-serializinghtml" "spyder-kernels" "sqlite" "stack-data" "text-unidecode" "textdistance" "threadpoolctl" "three-merge" "tinycss2" "tk" "tldextract" "toml" "tomler" "tomli" "tomlkit" "tornado" "traitlets" "tweepy" "twisted" "typing_extensions" "tzdata" "ujson" "unidecode" "urllib3" "virtualenv" "w3lib" "watchdog" "wcwidth" "webassets" "webencodings" "whatthepatch" "wheel" "wrapt" "wurlitzer" "xz" "yapf" "yarg" "zipp" "zlib" "zope-interface" "zstd"))

(defun +jg-python-exception-ivy ()
  (interactive)
  (insert (ivy-read "Exception Class: " +jg-python-ivy-exceptions :require-match t))
  )

(defun +jg-python-datetime-ivy ()
  (interactive)
  (insert (car (s-split " " (ivy-read "Datetime strptime: " +jg-python-ivy-datetimes :require-match t) t)))
  )

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
