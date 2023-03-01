# -*- mode:doot; -*-
"""


"""
# https://pydoit.org/
from __future__ import annotations
import pathlib as pl
import doot
from doot.tasks.groups import *
from doot.tasks.groups_secondary import *

from doot.tasks.builders.man import ManBuild

if __name__ == "dooter":
    # the equivalent of main
    manBuild = ManBuild(locs=doot.locs)
    pass
