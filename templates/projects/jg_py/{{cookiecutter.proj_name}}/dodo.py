# -*- mode:doit; -*-
##-- imports
import pathlib as pl
import shutil
import doot

##-- end imports

##-- config
with open("pyproject.toml", "r") as f:
    pyproject = toml.load(f)

src_dir       = pl.Path(pyproject['project']['name'])
build_dir     = pl.Path(pyproject['tool']['doit']['build_dir'])


DOIT_CONFIG = {
    "default_tasks" : ["default"],
    }

##-- end config

##-- post config doot imports

##-- end post config doot imports
