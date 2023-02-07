# -*- mode:doot; -*-
"""
Stub dooter file for task authoring

"""
# https://pydoit.org/
##-- imports
from __future__ import annotations
import pathlib as pl

import doot
from doot import globber
from doot.taskslib.data import images
from doot.taskslib.files import hashing

##-- end imports

from doot.taskslib.groups import *
from doot.taskslib.groups_secondary import *
from doot.taskslib.files.backup import BackupTask
from bkmkorg.doot_tasks.orgs import OrgMultiThreadCount, ThreadOrganise, ThreadListings

class TwitHash(hashing.HashAllFiles):
    """
    hash all thread files into a .hashes file
    """

    def __init__(self, name="twitter::hashes", locs=None, roots=None, rec=None):
        super().__init__(name, locs, roots or [locs.root], rec=rec)

    def filter(self, fpath):
        is_cache = fpath != pl.Path() and fpath.name[0] in "._"
        if is_cache:
            return False

        if "_files" not in fpath.name:
            return self.control.discard
        else:
            return self.control.keep

if __name__ == "dooter":
    # the equivalent of main
    hasher      = TwitHash(locs=doot.locs, rec=True)
    # hasher    = images.HashImages(doot.locs, rec=True)
    # ocr       = images.OCRGlobber(doot.locs)
    extractor   = ThreadListings(locs=doot.locs)
    threadcount = OrgMultiThreadCount(locs=doot.locs)
    threadorg   = ThreadOrganise(locs=doot.locs)
    backup      = BackupTask("twitter::backup", doot.locs, [doot.locs.root], output=doot.locs.backup)
    # gitlog = GitLogTask(locs=doot.locs)
