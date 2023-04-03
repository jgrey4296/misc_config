# -*- mode:doot; -*-
"""

"""
##-- imports
from __future__ import annotations

import logging as logmod
import pathlib as pl

import doot
from doit import create_after
from doit.action import CmdAction
from doit.task import clean_targets
from doit.tools import Interactive, PythonInteractiveAction, set_trace

##-- end imports

##-- logging
logging = logmod.getLogger(__name__)
##-- end logging

from doot.tasks.groups import *
from doot.tasks.groups_secondary import *
from doot.mixins.cargo import CargoMixin

if __name__ == "dooter":
    pass
