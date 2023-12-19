#!/usr/bin/env python3
"""

See EOF for license/metadata/notes as applicable
"""

##-- builtin imports
from __future__ import annotations

# import abc
import datetime
import enum
import functools as ftz
import itertools as itz
import logging as logmod
import pathlib as pl
import re
import time
import types
import weakref
# from copy import deepcopy
# from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable, Generator)
from uuid import UUID, uuid1

##-- end builtin imports

##-- lib imports
import more_itertools as mitz
##-- end lib imports

##-- logging
logging = logmod.getLogger(__name__)
##-- end logging

printer = logmod.getLogger("doot._printer")
from random import choice, choices

import doot
import doot.errors
import doot.utils.expansion as exp
from dootle.bibtex import middlewares as dmids
import bibtexparser as BTP
from bibtexparser import middlewares as ms

MYBIB                              = "#my_bibtex"
MAX_TAGS                           = 7
UPDATE        : Final[exp.DootKey] = exp.DootKey("update_")
FROM_KEY      : Final[exp.DootKey] = exp.DootKey("from")

def format_title(entry):
    fields = entry.fields_dict
    result = []

    year = "({})".format(fields['year'].value)

    match fields:
        case {"title": t, "subtitle": s}:
            result.append(f"{year} {t.value}: {s.value}")
        case {"title": t}:
            result.append(f"{year} {t.value}")

    if entry.entry_type == "book" and "volume" in fields:
        vol = fields['volume']
        result.append(f", Volume {vol.value}")

    return "".join(result)

def format_names(entry):
    fields = entry.fields_dict
    result = []
    et_al  = False
    editor = False

    names = []
    if "author" in fields and bool(fields['author'].value):
        names += fields['author'].value
    elif "editor" in fields and bool(fields['editor'].value):
        names += fields['editor'].value
        editor = True

    if len(names) > 2:
        names = names[:2]
        et_al = True

    for name in names: # transform the nameparts
        current = []
        if bool(name.von):
            current.append(" ".join(name.von))

        current.append(" ".join(name.last) + ",")

        if bool(name.jr):
            current.append(","+" ".join(name.jr))

        current.append(" ".join(name.first))
        result.append(" ".join(current))

    if et_al and editor:
        return " and ".join(result) + " et al. (ed.)"
    elif et_al:
        return " and ".join(result) + " et al."
    elif editor:
        return " and ".join(result) + " (ed.)"
    else:
        return " and ".join(result)

def format_for_mastodon(spec, state):
    data       = FROM_KEY.to_type(spec, state)
    update_key = UPDATE.redirect(spec)
    assert(isinstance(data, list))
    entry = choice(data)
    assert(isinstance(entry, BTP.model.Entry))

    fields = entry.fields_dict
    text = []
    text.append(format_title(entry))
    text.append(format_names(entry))
    match fields:
        case {"doi": doi_obj}:
            doi = doi_obj.value
            text.append(f"DOI: https://doi.org/{doi}")
        case {"url": url_obj}:
            url  = url_obj.value
            text.append(f"Url: {url}")
        case {"isbn": isbn_obj}:
            isbn = isbn_obj.value
            if "publisher" in fields:
                text.append(fields['publisher'].value)
            text.append(f"ISBN: {isbn}")
        case {"journal": job, "volume": vol, "number": num}:
            journal = job.value
            volume  = vol.value
            number = num.value
            text.append("{journal}: {volume}({number})")
        case _:
            printer.warning("Failed on: %s", entry)
            return False

    base_tags = fields['tags'].value
    if len(base_tags) > MAX_TAGS:
        base_tags = choices(base_tags, k=MAX_TAGS)

    htags = " ".join(map(lambda x: f"#{x}", sorted(base_tags)))
    text.append(f"{MYBIB} {htags}")

    if not bool(text):
        printer.warning("Failed on: %s", entry)
        return False

    return { update_key : "\n".join(text) }
