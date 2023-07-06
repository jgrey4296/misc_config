---
title: kpsewhich
status: draft
date: 2023-07-05
tags:
---

Usage: kpsewhich [OPTION]... [FILENAME]...

Standalone path lookup and expansion for the Kpathsea library.
The default is to look up each FILENAME in turn and report its
first match (if any) to standard output.

When looking up format (.fmt/.base/.mem) files, it is usually necessary
to also use -engine, or nothing will be returned; in particular,
-engine=/ will return matching format files for any engine.

-all                   output all matches, one per line (no effect with pk/gf).
[-no]-casefold-search  fall back to case-insensitive search if no exact match.
-cnf-line=STRING       parse STRING as a configuration file line.
-debug=NUM             set debugging flags.
-D, -dpi=NUM           use a base resolution of NUM; default 600.
-engine=STRING         set engine name to STRING.
-expand-braces=STRING  output variable and brace expansion of STRING.
-expand-path=STRING    output complete path expansion of STRING.
-expand-var=STRING     output variable expansion of STRING.
-format=NAME           use file type NAME (list shown by -help-formats).
-help                  display this message and exit.
-help-formats          display information about all supported file formats.
-interactive           ask for additional filenames to look up.
[-no]-mktex=FMT        disable/enable mktexFMT generation (FMT=pk/mf/tex/tfm).
-mode=STRING           set device name for $MAKETEX_MODE to STRING; no default.
-must-exist            search the disk as well as ls-R if necessary.
-path=STRING           search in the path STRING.
-progname=STRING       set program name to STRING.
-safe-in-name=STRING   check if STRING is ok to open for input.
-safe-out-name=STRING  check if STRING is ok to open for output.
-show-path=TYPE        output search path for file type TYPE
                         (list shown by -help-formats).
-subdir=STRING         only output matches whose directory ends with STRING.
-var-brace-value=STRING output brace-expanded value of variable $STRING.
-var-value=STRING       output variable-expanded value of variable $STRING.
-version               display version information number and exit.

Email bug reports to tex-k@tug.org.
Kpathsea home page: https://tug.org/kpathsea/
