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
from doot.tasks.files import listing
from code_analysis.doot_tasks import data as data_tasks
from code_analysis.doot_tasks import spider as spider_tasks

if __name__ == "dooter":
    # the equivalent of main
    # simple_listing = listing.SimpleListing("listing::originals", locs=doot.locs, focus=doot.locs.data / "originals")
    zip_data       = data_tasks.ZipData(locs=doot.locs)
    pop_sum        = data_tasks.TomlSummary(locs=doot.locs)
    toml_adjust    = data_tasks.TomlAdjust(locs=doot.locs, processor=data_tasks.TomlAdjust.adjust_listing)
    toml_zip_hash  = data_tasks.TomlAdjust("data::ziphash", locs=doot.locs, processor=data_tasks.TomlAdjust.add_zip_hash)
    toml_concat    = data_tasks.TomlConcat(locs=doot.locs)
    backup_zips    = data_tasks.BackupZips(locs=doot.locs)
    data_listings  = data_tasks.DataListing(locs=doot.locs)
    data_ages      = data_tasks.TomlAge(locs=doot.locs)
    data_tagless   = data_tasks.TomlTagless(locs=doot.locs)
    run_spider     = spider_tasks.RunSpider(locs=doot.locs)
