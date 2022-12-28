#!/usr/bin/env python3
# mode:doit; -*-
# https://pydoit.org/

##-- imports
import pathlib as pl
from doit.action import CmdAction
from doit import create_after
from doit.tools import set_trace, Interactive, PythonInteractiveAction
from doit.task import clean_targets
import doot
##-- end imports

##-- config
datatoml    = doot.setup_py()

DOIT_CONFIG = {
    "default_tasks" : [],
    "action_string_formatting" : "new",
    }

##-- end config

##-- post-config doot imports
from doot.files.clean_cache import CleanCacheAction, py_cache_globs
from doot.files.listall import task_listall
from doot.files.checkdir import CheckDir
from doot.utils import globber

from doot.groups import *
# from doot.groups_secondary import *
from doot.files.ziptask import ZipTask
##-- end post-config doot imports

all_checks = CheckDir.checkdir_group()
