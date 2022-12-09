# -*- mode:doit; -*-
##-- imports
import pathlib as pl
import shutil

from doit import create_after
from doit.action import CmdAction
from doit.tools import (Interactive, PythonInteractiveAction, create_folder,
                        set_trace)

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

src_dir       = pl.Path(pyproject['project']['name'])
build_dir     = pl.Path(pyproject['tool']['doit']['build_dir'])


DOIT_CONFIG = {
    "default_tasks" : ["default"],
    }

##-- end config

##-- util
def make_task(func):
    """ decorate a function to be a task-creator """
    func.create_doit_tasks = func
    return func

def build_cmd(cmd, args):
    return " ".join([cmd] + [str(x) for x in args])

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

    def __init__(self, cmd, *args, data=None, **kwargs):
        self.create_doit_tasks = self.make
        self.cmd               = cmd
        self.args              = args
        self.kwargs            = kwargs

    def make(self) -> dict:
        task_desc = self.kwargs.copy()
        task_desc['actions'] = [ build_cmd(self.cmd, self.args) ]
        return task_desc


##-- end util

##-- actions
## Can have auto filled parameters of:
## targets, dependencies, changed
## and 'task' gives access to all metadata

##-- end actions

##-- tasks

default = JGCmdTask("echo", "my blah default")

##-- end tasks

##-- setup
def _task_base_dircheck():
    """:: Create directories if they are missing """
    make = "mkdir -p %(targets)s"
    echo = "echo making %(targets)s"
    return {
        "actions"  : [ CmdAction(f"{make}; {echo}") ],
        "targets"  : [ build_dir ],
        "uptodate" : [ lambda task: all([pl.Path(x).exists() for x in task.targets]) ],
        "clean"    : [ force_clean_targets],
    }


##-- end setup

##-- building
install  = JGCmdTask("pip", "install", "--use-feature=in-tree-build", "--src", build_dir / "pip_temp", task_dep=["_task_base_dircheck"])
wheel    = JGCmdTask("pip", "wheel", "--use-feature=in-tree-build", "-w", build_dir / "wheel", "--use-pep517", "--src", build_dir / "pip_temp", ".", task_dep=["_task_base_dircheck"])
editlib  = JGCmdTask("pip", "install", "-e", task_dep=["_task_base_dircheck"])
srcbuild = JGCmdTask("pip",  "install", "--use-feature=in-tree-build", "-t", build_dir / "pip_src", "--src",  build_dir / "pip_temp", "-U",  ".", task_dep=["_task_base_dircheck"])

##-- end building

##-- testing
def task_test() -> dict:
    """:: Task definition """
    data = pyproject['tool']['doit']['commands']['test']
    cmd  = "python"
    args = ["-m", "unittest", "discover",
            pyproject['project']['name'],
            "-p", pyproject['tool']['doit']['commands']['test']['pattern'],
            "-t", pyproject['project']['name'],
            "-s", "{start}"
            ]

    if data['verbose']:
        args.append("-v")

    if data['failfast']:
        args.append('-f')

    return {
        ## Required (callable | tuple(callable, *args, **kwargs))
        "actions"     : [ build_cmd(cmd, args) ],
        "params"      : [ {"name"    : "start",
                           "start"   : "-s",
                           "default" : pyproject['project']['name']
                           },
                          ]
    }


##-- end testing

##-- linting

def task_initpy() -> dict:
    """:: touch all __init__.py files """
	# TODO find ${PY_TOP} -type d -print0 | xargs -0 -I {} touch "{}/__init__.py"

    cmd  = ""
    args = []
    return {
        ## Required (callable | tuple(callable, *args, **kwargs))
        "actions"     : [ build_cmd(cmd, args) ],
    }

def task_lint() -> dict:
    """:: lint the package """
    # TODO add ignore / ignore-patterns / --ignore-paths

    data = pyproject['tool']['doit']['commands']['lint']
    cmd  = data['executable']
    args = [
        "--output-format", data['outfmt'],
        "--output", data['outfile'],
        "-E" if data['errors'] else "",
        pyproject['project']['name']
    ]


    return {
        "actions"   : [ build_cmd(cmd, args) ],
        "verbosity" : 2,
        "targets"   : [ "package.lint" ],
        "clean"     : True,
    }


##-- end linting

##-- cleaning
uninstall = JGCmdTask("pip", "uninstall", "-y", pyproject['project']['name'])

def task_fresh() -> dict:
    """:: Clean the logs and caches"""
    # find ${PY_TOP} -regextype posix-egrep -regex .*\(.mypy_cache\|__pycache__\|flycheck_.+\)$)
    return {"actions" : ['find . -maxdepth 1 -name "log.*" -exec rm {}']}



##-- end cleaning

##-- documentation
def task_sphinx() -> dict:
    """:: Task definition """
    sphinx         = pyproject['tool']['sphinx']
    docs_dir       = sphinx['docs_dir']
    docs_build_dir = build_dir / docs_dir

    cmd  = "sphinx"
    args = ['-b', sphinx['builder'],
            docs_dir,
            docs_build_dir,
            ]

    match sphinx['verbosity']:
        case x if x > 0:
            args += ["-v" for i in range(x)]
        case x if x == -1:
            args.append("-q")
        case x if x == -2:
            args.append("-Q")

    return {
        "actions"  : [ build_cmd(cmd, args) ],
        "task_dep" : ["_task_base_dircheck"]
    }

def task_browse() -> dict:
    """:: Task definition """
    cmd  = "open"
    args = [ build_dir / "html" / "index.html" ]

    return {
        "actions"     : [build_cmd(cmd, args)],
        "task_dep"    : ["sphinx"],
    }


##-- end documentation

##-- reports
def task_requirements() -> dict:
    """:: generate requirements.txt """
    cmd  = "pip"
    args1 = ["freeze", "--all", "--exclude-editable",
             "-r", "requirements.txt", ">", "requirements.txt"]
    args2 = ["list", "--format=freeze", ">", "requirements.txt"]

    return {
        "actions" : [
            build_cmd(cmd, args1),
            build_cmd(cmd, args2),
        ],
        "targets" : [ "requirements.txt" ],
        "clean"   : True,
    }

## TODO bash -ic "conda list --export > ./conda_env.txt"
## TODO conda env export --from-history > env.yaml

# TODO class / line reports
def task_line_report():
    find_cmd = build_cmd("find",
                         [src_dir, "-name", '"*.py"',
                          "-not", "-name", '"test_*.py"',
                          "-not", "-name", '"*__init__.py"',
                          "-print0"])
    line_cmd = build_cmd("xargs", ["-0", "wc", "-l"])
    sort_cmd = build_cmd("sort", [])

    target = build_dir / "linecounts.report"

    return {
        "basename"  : "line-report",
        "actions"   : [ f"{find_cmd} | {line_cmd} | {sort_cmd} > {target}" ],
        "targets"   : [ target ],
        "task_dep"  : ["_task_base_dircheck"]
        "clean"     : True,
        "verbosity" : 2,
    }

# find {top} -name "*.py" -not -name "flycheck*" | xargs awk '/^class/ {print $0}' > class.report
##-- end reports

##-- tagging
def _task_tags_init():
    """:: initalise gtags """
    return {
        "actions" : [ f"gtags -C {pyproject['name']}" ],
        "targets" : [ src_dir / "GPATH", src_dir / "GRTAGS", src_dir / "GTAGS" ]
    }


def task_tags():
    """:: update tag files """
    return {
        "actions"   : [],
        "file_dep" : [ src_dir / "GPATH", src_dir / "GRTAGS", src_dir / "GTAGS" ]
    }


##-- end tagging
