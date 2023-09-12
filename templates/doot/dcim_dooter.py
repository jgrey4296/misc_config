# -*- mode:doot; -*-
"""
Dooter for image tasks

"""
# https://pydoit.org/
##-- imports
from __future__ import annotations
import pathlib as pl

import doot
##-- end imports

# from doot.tasks.groups import *
# from doot.tasks.groups_secondary import *
from doot.tasks.data import images
from doot.tasks.files import hashing
from doot.tasks.files import deleter
from doot.tasks.files.backup_collector import BackupCollectorTask

if __name__ == "dooter":
    ocr         = images.OCRGlobber(locs=doot.locs)
    hasher      = hashing.HashAllFiles(locs=doot.locs, roots=[doot.locs.data], rec=True)
    hash_group  = hashing.GroupHashes(locs=doot.locs)
    hash_dups   = hashing.DetectDuplicateHashes(locs=doot.locs)
    hash_clean  = hashing.MarkDuplicates(locs=doot.locs)
    hash_delete = deleter.DeleterTask(locs=doot.locs)
    backup      = BackupCollectorTask(locs=doot.locs, source=doot.locs.root, backup=doot.locs.backup)
