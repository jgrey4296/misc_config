# -*- mode:doot; -*-
"""
Dooter for easy access to regular tasks
https://pydoit.org/
"""
##-- imports
from __future__ import annotations
import pathlib as pl
import os
import doot
##-- end imports

from doot.tasks.bkmkorg import bookmark

print("pony dooter")
##-- bookmarks
bkmk_update    = bookmark.BookmarksUpdate(locs=doot.locs)
##-- end bookmarks
