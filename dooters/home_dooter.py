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

from doot import tasker, globber
from doot.tasks.files.backup import BackupTask
from doot.tasks.groups import *
from doot.tasks.groups_secondary import *
from doot.tasks.docs.logs import MoveLogs
from doot.tasks.bkmkorg import bibtex, basic, bookmark, tags, android
from doot.tasks.bkmkorg import socmedia_post as post
from doot.tasks.files.backup_collector import BackupCollectorTask
from doot.tasks.files.deleter import DeleterTask

if __name__ == "dooter":
    # the equivalent of main
    movelogs = MoveLogs(locs=doot.locs)

    time_announce  = basic.TimeAnnounce(locs=doot.locs)
    twitter_access = basic.TwitterAccess(locs=doot.locs)

    ##-- posting
    bib_post       = post.BibPoster(locs=doot.locs)
    img_post       = post.ImagePoster(locs=doot.locs)
    ##-- end posting

    ##-- bibtex
    bib_stub       = bibtex.BibtexStub(locs=doot.locs)
    bib_clean      = bibtex.BibtexClean(locs=doot.locs)
    ##-- end bibtex

    ##-- tags
    tag_clean      = tags.TagsCleaner(locs=doot.locs)
    tag_update     = tags.TagsIndexer(locs=doot.locs)
    tag_report     = tags.TagsReport(locs=doot.locs)
    ##-- end tags

    ##-- bookmarks
    bkmk_update    = bookmark.BookmarksUpdate(locs=doot.locs)
    ##-- end bookmarks

    ##-- backup
    backup_twitter = BackupCollectorTask("backup::twitter", locs=doot.locs, source=doot.locs.thread_library, backup=doot.locs.thread_backup)
    backup_pdfs    = BackupCollectorTask("backup::pdfs",    locs=doot.locs, source=doot.locs.pdfs,           backup=doot.locs.pdf_backup)
    backup_summary = BackupCollectorTask("backup::summary", locs=doot.locs, source=doot.locs.pdf_summary,    backup=doot.locs.pdf_summary_backup)
    backup_movies  = BackupCollectorTask("backup::movies",  locs=doot.locs, source=doot.locs.movies,         backup=doot.locs.movies_backup)
    backup_images  = BackupCollectorTask("backup::images",  locs=doot.locs, source=doot.locs.images,         backup=doot.locs.images_backup)

    ##-- end backup

    ##-- android
    adb_up         = android.ADBUpload(locs=doot.locs)
    adb_down       = android.ADBDownload(locs=doot.locs)
    adb_del        = android.ADBDelete(locs=doot.locs)
    ##-- end android

    deleter = DeleterTask(locs=doot.locs)
