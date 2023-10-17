# -*- mode: snippet -*-
# name  : import.profile
# uuid  : import.profile
# key   : import.profile
# group :
# --
##-- import profiling
import sys
import trace                    # stdlib. https://docs.python.org/3/library/trace.html
import tracemalloc              # human     = lambda x, y=False: tracemalloc._format_size(x, y)
import timeit                   # stdlib. https://docs.python.org/3/library/timeit.html
import linecache                # stdlib. https://docs.python.org/3/library/linecache.html
import difflib                  # stdlib. https://docs.python.org/3/library/difflib.html
import inspect                  # stdlib. https://docs.python.org/3/library/inspect.html
import gc                       # stdlib. garbage collector https://docs.python.org/3/library/gc.html
import ast                      # stdlib. https://docs.python.org/3/library/ast.html
import stackprinter             # https://github.com/cknd/stackprinter
import boltons.debugutils       # debugging          https://boltons.readthedocs.io/en/latest/debugutils.html
import boltons.ecoutils         # system analytics   https://boltons.readthedocs.io/en/latest/ecoutils.html
import boltons.gcutils          # garbage collection https://boltons.readthedocs.io/en/latest/gcutils.html
import boltons.tbutils          # tracebacks         https://boltons.readthedocs.io/en/latest/tbutils.html
import faulthandler             # for faulthandler.enable(file=sys.stderr, all_threads=True)
import signal                   #
import coverage                 #
##-- end import profiling
