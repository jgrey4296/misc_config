# -*- mode:python; -*-
"""
Stub dooter file for task authoring

"""
# https://pydoit.org/
##-- imports
from __future__ import annotations
import pathlib as pl
import doot
##-- end imports

from doot.tasks.files import listing

if __name__ == "dooter":
    # the equivalent of main
    # listings = list_all.FileListings(dirs=doot.locs, roots=[doot.locs.root])
    simple = listing.SimpleListing(locs=doot.locs)
    pass
