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

# from doot.tasks.groups import *
# from doot.tasks.groups_secondary import *
from doot.tasks.data import images
from doot.tasks.files import hashing

if __name__ == "dooter":
    # the equivalent of main
    ocr    = images.OCRGlobber(locs=doot.locs)
    # breakpoint()
    # pass
    hasher     = hashing.HashAllFiles(locs=doot.locs, roots=[doot.locs.data], rec=True)
    hash_group = hashing.GroupHashes(locs=doot.locs, roots=[doot.locs.root], rec=True)
    hash_dups  = hashing.DetectDuplicateHashes(locs=doot.locs)
    hash_clean = hashing.RemoveMissingHashes(locs=doot.locs)
    hash_delete = hashing.DeleteDuplicates(locs=doot.locs)
