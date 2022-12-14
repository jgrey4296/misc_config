# -*- mode:doit; -*-
# https://pydoit.org/

##-- imports
import pathlib as pl
import doot
##-- end imports

##-- config
with open("doot.toml", "r") as f:
    pyproject = toml.load(f)

build_dir = pl.Path("build")

DOIT_CONFIG = {
    "default_tasks" : [],
    }

##-- end config

##-- post config doot imports


##-- end post config doot imports
