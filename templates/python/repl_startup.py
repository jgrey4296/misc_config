#/usr/bin/env python3
"""

"""
##-- imports
from __future__ import annotations

import inspect
from os import getcwd
from re import Pattern
from types import ModuleType
from uuid import UUID, uuid1
from weakref import ref
import argparse
import datetime
import enum
import functools as ftz
import importlib
import itertools as itz
import logging as logmod
import pathlib as pl
import re
import sys
import time
import types
import typing
import uuid
import weakref
import atexit
import readline

import more_itertools as mitz
import pyparsing as pp

##-- end imports

logging = logmod.getLogger(__name__)

##-- readline history
## from: https://stackoverflow.com/questions/10346419
histfile = pl.Path.home() / ".cache/logs/python_history"
atexit.register(readline.write_history_file, histfile)
readline.set_history_length(500)

##-- end readline history

##-- argparse
#see https://docs.python.org/3/howto/argparse.html
parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog = "\n".join([""]))
parser.add_argument('--dir')
##-- end argparse
args = parser.parse_args()

initial_globals = set(globals().keys())

if args.dir:
    sys.path = [pl.Path(args.dir)] + sys.path
    print('Path Amended')

def cwd():
    return pl.Path(getcwd())

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


def api(obj, max=40):
    contents = sorted(dir(obj))
    dunders = [x for x in contents if x.startswith("__")]
    private = [x for x in contents if x.startswith("_") and not x.startswith("__")]
    public  = [x for x in contents if not x.startswith("_")]
    print("Dunders: ", end="")
    total = 0
    for x in dunders:
        if total > max:
            print("\n         ", end="")
            total = 0

        print(x, end=" ")
        total += len(x)

    print()
    print("Private: ", end="")
    total = 0
    for x in private:
        if total > max:
            print("\n         ", end=" ")
            total = 0
        print(x, end="")
        total += len(x)

    print()
    print("Public: ", end=" ")
    total = 0
    for x in public:
        if total > max:
            print("\n        ", end=" ")
            total = 0
        print(x, end=" ")
        total += len(x)

    print()

remind()

print(f"The __name__ of this repl is: {__name__}")
print(f"It is found at: {__file__}")
