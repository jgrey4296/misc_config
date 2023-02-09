# -*- mode:doot; -*-
"""
Dooter for image tasks

"""
# https://pydoit.org/
##-- imports
from __future__ import annotations
import pathlib as pl
from doit.action import CmdAction
from doit import create_after
from doit.tools import set_trace, Interactive, PythonInteractiveAction
from doit.task import clean_targets

import doot
##-- end imports

from doot.taskslib.groups import *
from doot.taskslib.groups_secondary import *
from doot.taskslib.data import images

if __name__ == "dooter":
    # the equivalent of main
    hasher = images.HashImages(locs=doot.locs)
    ocr    = images.OCRGlobber(locs=doot.locs)
