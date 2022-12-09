# -*- mode:doit; -*-
# https://pydoit.org/

##-- imports
import pathlib as pl
from doit.action import CmdAction
from doit import create_after
from doit.tools import set_trace, Interactive, PythonInteractiveAction
from doit.tasks import clean_targets
try:
    # For py 3.11 onwards:
    import tomllib as toml
except ImportError:
    # Fallback to external package
    import toml
##-- end imports

##-- config
with open("pyproject.toml", "r") as f:
    pyproject = toml.load(f)

build_dir = pl.Path("build")

DOIT_CONFIG = {
    "default_tasks" : [],
    }

##-- end config

##-- util
def make_task(func):
    """ decorate a function to be a task-creator """
    func.create_doit_tasks = func
    return func

def build_cmd(cmd, args):
    return " ".join([cmd] + args)


def force_clean_targets(task, dryrun):
    """ remove all targets from a task
    Add to a tasks 'clean' dict value
    """
    for target_s in sorted(task.targets, reverse=True):
        try:
            target = pl.Path(target_s)
            if dryrun:
                print("%s - dryrun removing '%s'" % (task.name, target))
                continue

            print("%s - removing '%s'" % (task.name, target))
            if target.is_file():
                target.remove()
            elif target.is_dir():
                shutil.rmtree(str(target))
        except OSError as err:
            print(err)
class JGCmdTask:
    """ Utility to create basic command line tasks in one line """

    def __init__(self, cmd, *args, **kwargs):
        self.create_doit_tasks = self.make
        self.cmd = cmd
        self.args = args
        self.kwargs = kwargs

    def make(self) -> dict:
        task_descr = self.kwargs.copy()
        task_desc['actions'] = [ build_cmd(self.cmd, self.args) ]
        return task_desc

##-- end util

##-- actions
## Can have auto filled parameters of:
## targets, dependencies, changed
## and 'task' gives access to all metadata

##-- end actions

##-- tasks

##-- end tasks
