# -*- mode:doot; -*-
"""

"""
from __future__ import annotations
import pathlib as pl
import doot
from doot.tasks.groups import *
from doot.tasks.groups_secondary import *

from doot.tasks.builders.man import ManBuild

if __name__ == "dooter":
    manBuild = ManBuild(locs=doot.locs)
    pass
