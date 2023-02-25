# -*- mode:doot; -*-
"""
Stub dooter file for task authoring

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

from doot.tasks.groups import *
from doot.tasks.groups_secondary import *

if __name__ == "dooter":
    # the equivalent of main
    pass
