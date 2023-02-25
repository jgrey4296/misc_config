# -*- mode:doot; -*-
"""
Dooter for twitter archive tasks

"""
# https://pydoit.org/
##-- imports
from __future__ import annotations

import pathlib as pl

import doot
from bkmkorg.doot_tasks.orgs import (OrgMultiThreadCount, ThreadListings,
                                     ThreadOrganise)
from doot import globber
from doot.mixins.commander import CommanderMixin
from doot.mixins.delayed import DelayedMixin
from doot.mixins.filer import FilerMixin
from doot.mixins.targeted import TargetedMixin
from doot.mixins.zipper import ZipperMixin
from doot.tasks.data import images
from doot.tasks.files import hashing
from doot.tasks.files.backup import BackupTask
from doot.tasks.groups import *
from doot.tasks.groups_secondary import *

##-- end imports






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



class TwitCompress(DelayedMixin, globber.DootEagerGlobber, ZipperMixin):
    """
    Compress each user directory to a zip file

    """
    def __init__(self, name="twitter::compress", locs=None, roots=None):
        super().__init__(name, locs, roots=roots or [locs.data], rec=True)

    def setup_detail(self, task):
        task.update({

        })


    def task_detail(self, task):
        task.update({
            "tasks" : [
                self.compress_threads,
            ],
        })
        return task

    def filter(self, fpath):
        if fpath.is_dir() and (fpath / ".tweets").exists():
            return self.control.keep
        return self.control.discard

    def compress_threads(self):


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
