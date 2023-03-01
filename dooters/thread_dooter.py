# -*- mode:doot; -*-
"""
Dooter for twitter archive tasks

"""
# https://pydoit.org/
from __future__ import annotations

import pathlib as pl

import doot
from doot import globber
from doot.mixins.commander import CommanderMixin
from doot.mixins.delayed import DelayedMixin
from doot.mixins.filer import FilerMixin
from doot.mixins.targeted import TargetedMixin
from doot.mixins.zipper import ZipperMixin

from doot.tasks.files import hashing
from doot.tasks.files.backup import BackupTask
from doot.tasks.groups import *
from doot.tasks.groups_secondary import *

from bkmkorg.doot_tasks.orgs import (OrgMultiThreadCount, ThreadListings,
                                     ThreadOrganise)

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

class TwitCompress(DelayedMixin, TargetedMixin, globber.DootEagerGlobber, ZipperMixin):
    """
    Compress each user directory to a zip file

    """

    def __init__(self, name="twitter::compress", locs=None, roots=None):
        super().__init__(name, locs, roots=roots or [locs.data], rec=True)

    def set_params(self):
        return self.target_params() + self.zip_params()

    def setup_detail(self, task):
        task.update({

        })
        return task

    def filter(self, fpath):
        if fpath.is_dir() and (fpath / ".tweets").exists():
            return self.control.keep
        return self.control.discard

    def subtask_detail(self, task, fpath):
        task.update({
            "actions" : [
                (self.compress_files, [fpath]),
                (self.compress_threads, [fpath]),
            ],
        })
        return task

    def compress_threads(self, fpath):
        zipf = fpath / f"{fpath.name}_threads.zip"
        self.zip_set_root(fpath)
        self.zip_create(zipf)
        self.zip_globs(zipf, "*.org", "*.html")


    def compress_files(self, fpath):
        file_dir = fpath / f"{fpath.name}_files"
        if not file_dir.exists():
            return

        zipf = file_dir.with_suffix(".zip")
        self.zip_set_root(fpath)
        self.zip_create(zipf)
        self.zip_globs(zipf, f"{file_dir.name}/*")

if __name__ == "dooter":
    # the equivalent of main
    try:
        from doot.tasks.data import images
        # hasher    = images.HashImages(doot.locs, rec=True)
        # ocr       = images.OCRGlobber(doot.locs)
    except ImportError as err:
        logging.warning("Import Failure: %s", err)
    # gitlog = GitLogTask(locs=doot.locs)
    hasher      = TwitHash(locs=doot.locs, rec=True)
    extractor   = ThreadListings(locs=doot.locs)
    threadcount = OrgMultiThreadCount(locs=doot.locs)
    threadorg   = ThreadOrganise(locs=doot.locs)
    backup      = BackupTask("twitter::backup", doot.locs, [doot.locs.root], output=doot.locs.backup)
    compress    = TwitCompress(locs=doot.locs)
