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
from random import choice

import doot
import doot.errors
from doot.utils.string_expand import expand_key

from dootle.bibtex.field_cleaner_mixin import BibFieldCleanMixin

cleaner = BibFieldCleanMixin()
MYBIB = "#my_bibtex"

def format_title(entry):
    result = [entry['title']]
    subtitle = entry.get("subtitle", None)
    volume = entry.get("volume", None)

    if subtitle:
        result.append(": ")
        result.append(subtitle)
    if volume:
        result.append(f", Volume {volume}")

    return "".join(result)

def format_names(names):
    if len(names) > 2:
        as_str = format_names(names[:2])
        return f"{as_str} et al."

    flattened = []
    for name in names:
        von = "von " if bool(name.get('von', None)) else ""
        jr  = " jr., " if bool(name.get("jr", None)) else ", "
        last    = name.get("last", [])
        first   = name.get("first", [])

        if bool(last):
            last = " ".join(last)
        if bool(first):
            first = " ".join(first)
        flattened.append(f"{von}{last}{jr}{first}")

    return " and ".join(flattened)

def format_for_mastodon(spec, state):
    data       = expand_key(spec.kwargs.on_fail("from").from_(), spec, state)
    update_key = spec.kwargs.on_fail("text").update_()
    assert(isinstance(data, list))
    entry = choice(data)
    assert(isinstance(entry, dict))
    cleaner.bc_split_names(entry)
    text = []
    match entry:
        case {"__split_names" : key, "url": url, "year": year, "tags": tags}:
            text.append(format_title(entry))
            text.append(f"({year}) " + format_names(entry[f"__{key}"]))
            text.append(f"Url: {url}")
            htags = "#" + " #".join(tags.split(","))
            text.append(f"{MYBIB} {htags}")
        case {"__split_names" : key, "isbn": isbn, "year": year, "tags": tags}:
            text.append(format_title(entry))
            text.append(f"({year}) " + format_names(entry[f"__{key}"]))
            if "publisher" in entry:
                text.append(entry['publisher'])
            text.append(f"ISBN: {isbn}")
            htags = "#" + " #".join(tags.split(","))
            text.append(f"{MYBIB} {htags}")
        case { "__split_names" : key, "doi": doi, "year": year, "tags": tags, "journal": journal}:
            text.append(format_title(entry))
            text.append(f"({year}) " + format_names(entry[f"__{key}"]))
            text.append("{journal}")
            text.append(f"Doi: {doi}")
            htags = "#" + " #".join(tags.split(","))
            text.append(f"{MYBIB} {htags}")

    if not bool(text):
        printer.warning("Failed on: %s", entry)
        printer.warning("- Title: %s", "title" in entry)
        printer.warning("- Year: %s", "year" in entry)
        printer.warning("- isbn: %s", "isbn" in entry)
        printer.warning("- url: %s", "url" in entry)
        printer.warning("- doi: %s", "doi" in entry)
        printer.warning("- journal: %s", "journal" in entry)
        return False

    return { update_key : "\n".join(text) }

def select_one_entry(spec, state):
    bib_db     = expand_key(spec.kwargs.on_fail("from").from_(), spec, state)
    update_key = spec.kwargs.on_fail("entry").update_()
    entries    = bib_db.entries
    entry      = choice(entries)
    # TODO have white/black list

    if bool(entry):
        return {update_key : entry}

def pretend_post(spec, state):
    text = expand_key(spec.kwargs.on_fail("text").from_(), spec, state)
    printer.info("Would Be Posting:\n%s", text)

"""


"""
