#/usr/bin/env python3
"""

"""
##-- imports
from __future__ import annotations

from types import ModuleType
import argparse
import functools
import itertools
import logging as logmod
import pathlib as pl
import re
import sys
import importlib
from os import getcwd
from re import Pattern
from uuid import UUID, uuid1
from weakref import ref

import pyparsing as pp

##-- end imports

##-- logging
logging = logmod.getLogger(__name__)
##-- end logging

##-- argparse
#see https://docs.python.org/3/howto/argparse.html
parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog = "\n".join([""]))
parser.add_argument('--dir')
##-- end argparse
args = parser.parse_args()

initial_globals = set(globals().keys())

if args.dir:
    sys.path = [pathlib.Path(args.dir)] + sys.path
    print('Path Amended')

def cwd():
    return pathlib.Path(getcwd())


def logattrs():
    logrecord = logmod.makeLogRecord({})
    attrs = [x for x in dir(logrecord) if not x.startswith("__")]
    return attrs

def remind():
    current_globals = set(globals().keys()).difference(initial_globals)
    print("Reminder: [cwd, logattrs, reload, pp, remind, itertools, functools, re] are loaded")
    print(f"Globals since load: {current_globals}")

def reload():
    for name, x in globals().items():
        if isinstance(x, ModuleType):
            try:
                print(f"Reloading: {name}")
                importlib.reload(x)
            except Exception as err:
                print(err)


remind()
