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

##-- config
DOIT_CONFIG = {
    "action_string_formatting" : "new",
    "default_tasks"            : [],
}

##-- end config

##-- post-config doot imports
# from doot.tasks.groups import *
# from doot.tasks.groups_secondary import *
##-- end post-config doot imports

from doot.tasks.files import listing

if __name__ == "dooter":
    # the equivalent of main
    # listings = list_all.FileListings(dirs=doot.locs, roots=[doot.locs.root])
    simple = listing.SimpleListing(locs=doot.locs)
    pass
