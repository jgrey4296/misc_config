#/usr/bin/env python3
"""

"""
##-- imports
from __future__ import annotations

from os import getcwd
import pathlib
import abc
import re
import argparse
import logging as logmod
import sys
from copy import deepcopy
from dataclasses import InitVar, dataclass, field
from importlib import reload
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from uuid import UUID, uuid1
from weakref import ref
import pyparsing as pp

if TYPE_CHECKING:
    # tc only imports
    pass
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
#args.aBool...

if args.dir:
    sys.path = [pathlib.Path(args.dir)] + sys.path
    print('Path Amended')

def cwd():
    return pathlib.Path(getcwd())


def logattrs():
    logrecord = logmod.makeLogRecord({})
    attrs = [x for x in dir(logrecord) if not x.startswith("__")]
    return attrs


print("Reminder: [cwd, logattrs, reload, pp] are loaded")
