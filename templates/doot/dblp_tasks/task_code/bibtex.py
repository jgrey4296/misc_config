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
from urllib.parse import urlparse

import doot
import doot.errors
from doot.structs import DootKey
from dootle.bibtex import middlewares as dmids
import bibtexparser as BTP
from bibtexparser import middlewares as ms
from bibtexparser.middlewares.middleware import BlockMiddleware

MYBIB                              = "#my_bibtex"
MAX_TAGS                           = 7
UPDATE        : Final[DootKey]     = DootKey.make("update_")
FROM_KEY      : Final[DootKey]     = DootKey.make("from")
FPATH                              = DootKey.make("fpath")
LIB_ROOT                           = DootKey.make("lib_root")

class MergeMultipleAuthorsEditors(BlockMiddleWare):
    """ Merge multiple fields of the same name """

    def transform_entry(self, entry, library):
        pass

class LockCrossrefKeys(BlockMiddleWare):
    """ ensure crossref consistency by appending _ to keys """

    def transform_entry(self, entry, library):
        pass

def build_parse_stack(spec, state):
    read_mids = [
        ms.ResolveStringReferencesMiddleware(True),
        ms.RemoveEnclosingMiddleware(True),
        dmids.FieldAwareLatexDecodingMiddleware(True, keep_braced_groups=True, keep_math_mode=True),
        # dmids.ParsePathsMiddleware(lib_root=LIB_ROOT.to_path(spec, state),
        dmids.ParseTagsMiddleware(),
        ms.SeparateCoAuthors(True),
        dmids.RelaxedSplitNameParts(True),
        dmids.TitleStripMiddleware(True)
    ]
    return {spec.kwargs.update_ : read_mids}

def build_simple_parse_stack(spec, state):
    read_mids = [
        ms.ResolveStringReferencesMiddleware(True),
        ms.RemoveEnclosingMiddleware(True),
        dmids.FieldAwareLatexDecodingMiddleware(True, keep_braced_groups=True, keep_math_mode=True),
        # dmids.ParsePathsMiddleware(lib_root=LIB_ROOT.to_path(spec, state),
        # dmids.ParseTagsMiddleware(),
        # ms.SeparateCoAuthors(True),
        # dmids.RelaxedSplitNameParts(True),
        dmids.TitleStripMiddleware(True)
    ]
    return {spec.kwargs.update_ : read_mids}
def build_simple_write_stack(spec, state):
    write_mids = [
        # dmids.MergeLastNameFirstName(True),
        # ms.MergeCoAuthors(True),
        MergeMultipleAuthorsEditors(True),
        LockCrossrefKeys(True),
        dmids.FieldAwareLatexEncodingMiddleware(keep_math=True, enclose_urls=False),
        dmids.WriteTagsMiddleware(),
        # dmids.WritePathsMiddleware(lib_root=LIB_ROOT.to_path(spec, state))
        ms.AddEnclosingMiddleware(allow_inplace_modification=True, default_enclosing="{", reuse_previous_enclosing=False, enclose_integers=True),
    ]
    return {spec.kwargs.update_ : write_mids}



def map_urls(spec, state):
    base    = FPATH.to_path(spec, state)
    db      = FROM_KEY.to_type(spec, state)
    update  = UPDATE.redirect(spec)
    mapping = {}
    for entry in db.entries:
        if entry.entry_type.lower() == "inproceedings":
            mapping = {}
            break

        if entry.entry_type.lower() != "proceedings":
            continue

        fields = entry.fields_dict
        url    = fields.get('biburl')
        if url is not None:
            key = urlparse(url.value).path.removesuffix(".bib").removeprefix("/rec/")
            mapping[key] = base
            continue

    if len(mapping) > 1:
        mapping = {x:y.with_stem("gen_" + x.split("/")[-1]) for x,y in mapping.items()}

    printer.info("-- Mapped %s keys", len(mapping))
    return { update : mapping }

def join_mappings(spec, state):
    mappings : list[dict] = FROM_KEY.to_type(spec, state)
    update = UPDATE.redirect(spec)
    total = {}
    for mapping in mappings:
        conflict = set(total.keys()).intersection(mapping.keys())
        if bool(conflict):
            raise doot.errors.DootActionError("Conflicting Mapping", conflict)
        total.update(mapping)

    printer.warning("-- Collected %s mappings : %s", len(total), list(total.keys()))
    return { update : total }



def insert_entries(spec, state):
    fpath   = DootKey.make("fpath").to_path(spec, state)
    update  = UPDATE.redirect(spec)
    db      = UPDATE.to_type(spec, state)
    entries = FROM_KEY.to_type(spec, state)
    db.add(entries)

    return { update : db, "fstem" : fpath.stem }
