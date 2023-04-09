# -*- mode:doot; -*-
"""

"""
##-- imports
from __future__ import annotations
import pathlib as pl
import sys
import shutil
import logging as logmod

import doot
##-- end imports

##-- logging
logging = logmod.getLogger(__name__)
##-- end logging

from doot.tasks.groups import *
# from doot.tasks.groups_secondary import *

from doot.tasks.bkmkorg import basic, bibtex, bookmark, tags
from doot.tasks.builders.pelican import PelicanTasker, PelicanServer

if __name__ == "dooter":
    # noscript     = basic.NoScriptMerge(locs=doot.locs)

    cleaner      = bibtex.BibtexClean(locs=doot.locs)
    report       = bibtex.BibtexReport(locs=doot.locs)
    stubber      = bibtex.BibtexStub(locs=doot.locs)
    pdflib_clean = bibtex.LibDirClean(locs=doot.locs)

    bkmks_update = bookmark.BookmarksUpdate(locs=doot.locs)
    bkmk_clean   = bookmark.BookmarksCleaner(locs=doot.locs)
    bkmk_report  = bookmark.BookmarksReport(locs=doot.locs)

    tags_clean   = tags.TagsCleaner(locs=doot.locs)
    tags_report  = tags.TagsReport(locs=doot.locs)
    tags_index   = tags.TagsIndexer(locs=doot.locs)

    web_build    = PelicanTasker(locs=doot.locs)
    web_serve    = PelicanServer(locs=doot.locs)

    # TODO report
    # TODO indexer
    pass
