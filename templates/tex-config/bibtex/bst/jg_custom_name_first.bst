%%-- preface
%%
%% This is file `jg_custom.bst',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% merlin.mbs  (with options: `ay,cn,annote,seq-yr,ed-au,nmlm,x4,m1,keyxyr,note-yr,atit-u,num-xser,isbn,issn,doi,xedn,revdata,eprint,url,url-blk,nfss,')
%% ----------------------------------------
%% ***  ***
%%
%% Copyright 1994-2011 Patrick W Daly
 % ===============================================================
 % IMPORTANT NOTICE:
 % This bibliographic style (bst) file has been generated from one or
 % more master bibliographic style (mbs) files, listed above.
 %
 % This generated file can be redistributed and/or modified under the terms
 % of the LaTeX Project Public License Distributed from CTAN
 % archives in directory macros/latex/base/lppl.txt; either
 % version 1 of the License, or any later version.
 % ===============================================================
 % Name and version information of the main mbs file:
 % \ProvidesFile{merlin.mbs}[2011/11/18 4.33 (PWD, AO, DPC)]
 %   For use with BibTeX version 0.99a or later
 %-------------------------------------------------------------------
 % This bibliography style file is intended for texts in ENGLISH
 % This is an author-year citation style bibliography. As such, it is
 % non-standard LaTeX, and requires a special package file to function properly.
 % Such a package is    natbib.sty   by Patrick W. Daly
 % or:                  authordate1-4.sty
 % The form of the bibitem entries is
 %   \bibitem[\protect\citename{Jones et al.}1990]{key}...
 %---------------------------------------------------------------------

%%-- end preface

%%-- init
ENTRY
  { % Standard:
    address
    archive
    author
    booktitle
    chapter
    collaboration
    edition
    editor
    institution
    isbn
    issn
    journal
    key
    month
    note
    number
    pages
    publisher
    series
    title
    volume
    year
    %% Custom -------
    republished
    annotation
    eid
    eprint
    tags
    doi
    bookauthor
    bookpublisher
    % thesis type: (phd, masters)
    type
    % for software, the system it runs on:
    platform
    url
    % For judicial opinions, legislation, standards etc:
    status
    plaintiff
    defendant
    short_parties
    counsel
    dissent
    concur
    country
    % casenumber, statutory instrument, HR..., etc:
    identifier
    % for music scores:
    arrangement
    part
    % for both music and judicial, legislation etc
    section
  }
  {}
  { label extra.label sort.label short.list }
INTEGERS { output.state before.all mid.sentence after.sentence after.block }

STRINGS { s t }
INTEGERS { tempctra tempctrb }

FUNCTION {init.state.consts}
{ #0 'before.all :=
  #1 'mid.sentence :=
  #2 'after.sentence :=
  #3 'after.block :=
}

STRINGS { last.label next.extra }
INTEGERS { last.extra.num last.extra.num.extended last.extra.num.blank number.label }
FUNCTION {init.extra.label.stuff}
{ #0 int.to.chr$ 'last.label :=
  "" 'next.extra :=
  #0 'last.extra.num :=
  "a" chr.to.int$ #1 - 'last.extra.num.blank :=
  last.extra.num.blank 'last.extra.num.extended :=
  #0 'number.label :=
}
%%-- end init

%%-- macros
MACRO {jan} {"January"}

MACRO {feb} {"February"}

MACRO {mar} {"March"}

MACRO {apr} {"April"}

MACRO {may} {"May"}

MACRO {jun} {"June"}

MACRO {jul} {"July"}

MACRO {aug} {"August"}

MACRO {sep} {"September"}

MACRO {oct} {"October"}

MACRO {nov} {"November"}

MACRO {dec} {"December"}

MACRO {acmcs} {"ACM Computing Surveys"}

MACRO {acta} {"Acta Informatica"}

MACRO {cacm} {"Communications of the ACM"}

MACRO {ibmjrd} {"IBM Journal of Research and Development"}

MACRO {ibmsj} {"IBM Systems Journal"}

MACRO {ieeese} {"IEEE Transactions on Software Engineering"}

MACRO {ieeetc} {"IEEE Transactions on Computers"}

MACRO {ieeetcad} {"IEEE Transactions on Computer-Aided Design of Integrated Circuits"}

MACRO {ipl} {"Information Processing Letters"}

MACRO {jacm} {"Journal of the ACM"}

MACRO {jcss} {"Journal of Computer and System Sciences"}

MACRO {scp} {"Science of Computer Programming"}

MACRO {sicomp} {"SIAM Journal on Computing"}

MACRO {tocs} {"ACM Transactions on Computer Systems"}

MACRO {tods} {"ACM Transactions on Database Systems"}

MACRO {tog} {"ACM Transactions on Graphics"}

MACRO {toms} {"ACM Transactions on Mathematical Software"}

MACRO {toois} {"ACM Transactions on Office Information Systems"}

MACRO {toplas} {"ACM Transactions on Programming Languages and Systems"}

MACRO {tcs} {"Theoretical Computer Science"}

MACRO {hor} {"House of Representatives"}

MACRO {sen} {"Senate"}

MACRO {hol} {"House of Lords"}

MACRO {hoc} {"House of Commons"}



%%-- end macros

%%-- utils
FUNCTION { debug } % {str}
% { "Debug: " swap$ * top$ }
{ pop$ }

function { debug_top } % str
{ duplicate$
  "Debug: Top of stack is " swap$ * top$
}


FUNCTION {not} % {val}
{   { #0 }
    { #1 }
  if$
}

FUNCTION {and} % {val, val}
{   'skip$
    { pop$ #0 }
  if$
}

FUNCTION {or} % {val, val}
{   { pop$ #1 }
    'skip$
  if$
}

FUNCTION {emphasize} % {str}
{ duplicate$ empty$
    { pop$ "" }
    { "\emph{" swap$ * "}" * }
  if$
}

FUNCTION {capitalize} % {str}
{ "u" change.case$ "t" change.case$ }

FUNCTION {space.word} % {str}
{ " " swap$ * " " * }


INTEGERS { len }
FUNCTION {chop.word} % {str, len, ?}
{ 's :=
  'len :=
  s #1 len substring$ =
    { s len #1 + global.max$ substring$ }
    's
  if$
}

FUNCTION {sortify} % {str}
{ purify$
  "l" change.case$
}


FUNCTION {field.or.null} % {str}
{ duplicate$ empty$
    { pop$ "" }
    'skip$
  if$
}

FUNCTION {tie.or.space.prefix} % { str }
{ duplicate$ text.length$ #3 <
    { "~" }
    { " " }
  if$
  swap$
}

FUNCTION {bibinfo.check} % {key, str}
{ swap$
  duplicate$ missing$
    {
      pop$ pop$
      ""
    }
    { duplicate$ empty$
        {
          pop$ pop$
          ""
        }
        { swap$ pop$ }
      if$
    }
  if$
}

FUNCTION {bibinfo.warn} % { key str }
{ swap$
  duplicate$ missing$
    {
      swap$
      "Bibinfo.warn: missing " swap$ * " in " * cite$ * warning$
      pop$
      ""
    }
    { duplicate$ empty$
        {
          swap$
          "Bibinfo.warn: empty " swap$ * " in " * cite$ * warning$
          pop$
          ""
        }
        { swap$ pop$ }
      if$
    }
  if$
}

FUNCTION {add.blank} % { str }
{  " " * before.all 'output.state := }


FUNCTION {n.dashify} % {str}
{
  't :=
  ""
    { t empty$ not }
    { t #1 #1 substring$ "-" =
        { t #1 #2 substring$ "--" = not
            { "--" *
              t #2 global.max$ substring$ 't :=
            }
            {   { t #1 #1 substring$ "-" = }
                { "-" *
                  t #2 global.max$ substring$ 't :=
                }
              while$
            }
          if$
        }
        { t #1 #1 substring$ *
          t #2 global.max$ substring$ 't :=
        }
      if$
    }
  while$
}

FUNCTION {either.or.check} % { str, str }
{ empty$
    'pop$
    { "Bibinfo.structure: Can't use both " swap$ * " fields in " * cite$ * warning$ }
  if$
}

INTEGERS { multiresult }
FUNCTION {multi.page.check}
{ 't :=
  #0 'multiresult :=
    { multiresult not
      t empty$ not
      and
    }
    { t #1 #1 substring$
      duplicate$ "-" =
      swap$ duplicate$ "," =
      swap$ "+" =
      or or
        { #1 'multiresult := }
        { t #2 global.max$ substring$ 't := }
      if$
    }
  while$
  multiresult
}
%%-- end utils

%%-- output
FUNCTION {output.nonnull} % {str}
{ 's :=
  duplicate$ "Writing: " swap$ * debug
  output.state mid.sentence =
    {", " * write$ }
    { output.state after.block =
        { add.period$ "\\*" * write$ newline$
          % "\newblock " write$
        }
        { output.state before.all =
            'write$
            { add.period$ " " * write$ }
          if$
        }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
  duplicate$ "Queued: " swap$ * debug
}

FUNCTION {output} % {str}
{ duplicate$ empty$
    'pop$
    'output.nonnull
  if$
}

FUNCTION {output.check} % {str, errmsg}
{ 't :=
  duplicate$ empty$
    { pop$ "Output.check: empty " t * " in " * cite$ * warning$ }
    'output.nonnull
  if$
}

FUNCTION {output.bibitem}
{ "------ Starting Item: " cite$ " of type: " type$ * * * debug
  newline$
  "\begin{samepage}" write$ newline$
  "\bibitem[" year "]" * * write$
  "{" cite$ "}" * * write$
  newline$
  "" write$
  before.all 'output.state :=
  " "
}
%%-- end output

%%-- new creation
FUNCTION {new.block}
{ output.state before.all =
    'skip$
    { after.block 'output.state := }
  if$
}

FUNCTION {new.sentence}
{ output.state after.block =
    'skip$
    { output.state before.all =
        'skip$
        { after.sentence 'output.state := }
      if$
    }
  if$
}

FUNCTION {date.block}
{ new.block }

FUNCTION {new.block.checkb}
{ empty$
  swap$ empty$
  and
    'skip$
    'new.block
  if$
}
%%-- end new creation

%%-- tags
% from https://tex.stackexchange.com/questions/474656/
FUNCTION {tags.split.inner}
{
  's :=
  "" 't :=
  #1 'tempctra  :=
  { s empty$ not
    tempctra and }
  {s #1 #1 substring$ "," =
     {
       #0 'tempctra :=
       s #1 global.max$ substring$ 's := }
     {
       t s #1 #1 substring$ * 't :=
     }
   if$
    s #2 global.max$ substring$ 's :=
    }
  while$
  s
  t
}
FUNCTION {tags.split.outer} % {str }
{
  's :=
  ""
  #0 'tempctrb  :=
  {s empty$ not }
    { s tags.split.inner
      't :=
      's :=
      tempctrb #0 =
        { }
        {", " * * }
      if$
      tempctrb #1 + 'tempctrb  :=
      t
    }
  while$
  *
}
FUNCTION {format.tags} % { str }
{ duplicate$ empty$
    { skip$ }
    { new.block
      tags.split.outer
      "\begin{lstlisting}[breaklines=true,breakatwhitespace=true]\n"
      "Tags: "  *
      swap$ * "\n" *
      "\end{lstlisting}"  *
    }
  if$
}


%%-- end tags

%%-- localization
%%
% Each function has a name bbl.xxx where xxx is the English word.
% The language selected here is ENGLISH

FUNCTION {bbl.and}
{ "and"}


FUNCTION {bbl.etal}
{ "et~al." }


FUNCTION {bbl.editors}
{ "editors" }


FUNCTION {bbl.editor}
{ "editor" }


FUNCTION {bbl.edby}
{ "edited by" }


FUNCTION {bbl.edition}
{ "edition" }


FUNCTION {bbl.volume}
{ "volume" }


FUNCTION {bbl.of}
{ "of" }


FUNCTION {bbl.number}
{ "number" }


FUNCTION {bbl.nr}
{ "no." }


FUNCTION {bbl.in}
{ "in" }


FUNCTION {bbl.pages}
{ "pages" }


FUNCTION {bbl.page}
{ "page" }


FUNCTION {bbl.eidpp}
{ "pages" }


FUNCTION {bbl.chapter}
{ "chapter" }


FUNCTION {bbl.techrep}
{ "Technical Report" }


FUNCTION {bbl.mthesis}
{ "Master's thesis" }


FUNCTION {bbl.phdthesis}
{ "Ph.D. thesis" }


FUNCTION {bbl.thesis}
{ "Thesis" }

FUNCTION {get.bbl.editor}
{ editor num.names$ #1 > 'bbl.editors 'bbl.editor if$ }

%%-- end localization

%%-- format
FUNCTION {format.url} % { str }
{ duplicate$ empty$
    { skip$ }
    { "\urlprefix\url{" swap$ * "}" * }
  if$
}

FUNCTION {fin.entry} % { nil }
{ duplicate$ "Writing Output: " swap$ * debug
  add.period$ write$ newline$
  url format.url duplicate$ empty$ 'pop$
  { "Adding Url" debug
    write$ newline$
  }
  if$
  tags "tags" bibinfo.warn format.tags duplicate$ empty$ 'pop$
  { "Adding Tags" debug
    write$ newline$
  }
  if$
  "\end{samepage}" write$ newline$
  "\pagebreak[1]" write$ newline$
  "------ Finished Entry: " cite$ * debug
}

FUNCTION {format.eprint}
{ eprint duplicate$ empty$
    'skip$
    { "\eprint"
      archive empty$
        'skip$
        { "[" * archive * "]" * }
      if$
      "{" * swap$ * "}" *
    }
  if$
}


FUNCTION {format.isbn} % {str}
{ duplicate$ empty$
    { skip$ }
    {
      new.block
      "ISBN " swap$ *
    }
  if$
}


FUNCTION {format.issn}
{ issn duplicate$ empty$ 'skip$
    {
      new.block
      "ISSN " swap$ *
    }
  if$
}


FUNCTION {format.doi} % {str}
{ duplicate$ empty$
    { skip$ }
    {
      new.block
      "\doi{" swap$ * "}" *
    }
  if$
}

INTEGERS { nameptr namesleft numnames }
STRINGS  { bibinfo}
FUNCTION {format.names}
{ 'bibinfo :=
  duplicate$ empty$ 'skip$ {
  's :=
  "" 't :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
      "{ff~}{vv~}{ll}{, jj}"
      format.name$
      bibinfo bibinfo.check
      't :=
      nameptr #1 >
        {
          nameptr #1
          #1 + =
          numnames #4
          > and
            { "others" 't :=
              #1 'namesleft := }
            'skip$
          if$
          namesleft #1 >
            { ", " * t * }
            {
              s nameptr "{ll}" format.name$ duplicate$ "others" =
                { 't := }
                { pop$ }
              if$
              numnames #2 >
                { "," * }
                'skip$
              if$
              t "others" =
                {
                  " " * bbl.etal *
                }
                {
                  bbl.and
                  space.word * t *
                }
              if$
            }
          if$
        }
        't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
  } if$
}

FUNCTION {format.key}
{ author empty$
  { editor empty$
      { key field.or.null }
      { "" }
    if$
  }
  { key field.or.null }
  if$
}

FUNCTION {format.authors} % str
{ author "author" format.names
  duplicate$ empty$ 'skip$
  {   }
  if$
}


FUNCTION {format.editors}
{ editor "editor" format.names
  duplicate$ empty$
    'skip$
    {
      ", " *
      " " *
      get.bbl.editor
      *
    }
  if$
}

FUNCTION {format.authors.andor.editors}
{ "Formatting Auths/Eds" debug
  author empty$
    { "" }
    { format.authors }
  if$
  collaboration missing$
    { "" }
    { collaboration empty$ { " (" collaboration * ")" * * } { "" } if$ }
  if$
  author empty$ editor empty$ and not
    { "" }
    { ", edited by " }
  if$
  editor empty$
    { "" }
    { editor "editor" format.names }
  if$
  * * *
}

FUNCTION {format.note}
{
 note empty$
    { "" }
    { note #1 #1 substring$
      duplicate$ "{" =
        'skip$
        { output.state mid.sentence =
          { "l" }
          { "u" }
        if$
        change.case$
        }
      if$
      note #2 global.max$ substring$ * "note" bibinfo.warn
    }
  if$
}

FUNCTION {word.in}
{ bbl.in capitalize
  " " *
}

% TODO format the year and republished year together
FUNCTION {format.date} % { str }
{ duplicate$ empty$
    {
      "empty year in " cite$ * "; set to ????" * warning$
       pop$ "????"
    }
    'skip$
  if$
  extra.label *
}

FUNCTION {format.title}
{ "Formating Title" debug
  duplicate$ empty$ 'skip$
    {
      emphasize
    }
  if$
}

FUNCTION {format.bvolume}
{ volume empty$
    { "" }
    { bbl.volume volume tie.or.space.prefix * *
      series
      duplicate$ empty$ 'pop$
        { swap$ bbl.of space.word * swap$
          emphasize * }
      if$
      "volume and number" number either.or.check
    }
  if$
}

FUNCTION {format.number.series}
{ volume empty$
    { number empty$
        { series field.or.null }
        { series empty$
            { number }
            { output.state mid.sentence =
                { bbl.number }
                { bbl.number capitalize }
              if$
              number tie.or.space.prefix * *
              bbl.in space.word *
              series *
            }
          if$
        }
      if$
    }
    { "" }
  if$
}


FUNCTION {format.edition}
{ duplicate$ empty$ 'skip$
    {
      output.state mid.sentence =
        { "l" }
        { "t" }
      if$ change.case$
      "edition" bibinfo.warn
      " " * bbl.edition *
    }
  if$
}


FUNCTION {format.pages}
{ pages duplicate$ empty$ 'skip$
    { duplicate$ multi.page.check
        {
          bbl.pages swap$
          n.dashify
        }
        {
          bbl.page swap$
        }
      if$
      tie.or.space.prefix
      "pages" bibinfo.warn
      * *
    }
  if$
}

FUNCTION {format.journal.pages}
{ pages duplicate$ empty$ 'pop$
    { swap$ duplicate$ empty$
        { pop$ pop$ format.pages }
        {
          ":" *
          swap$
          n.dashify
          "pages" bibinfo.warn
          *
        }
      if$
    }
  if$
}

FUNCTION {format.journal.eid}
{ eid "eid" bibinfo.warn
  duplicate$ empty$ 'pop$
    { swap$ duplicate$ empty$ 'skip$
      {
          ":" *
      }
      if$
      swap$ *
    }
  if$
}

FUNCTION {format.vol.num.pages}
{ volume field.or.null
  number "number" bibinfo.warn duplicate$ empty$ 'skip$
    {
      swap$ duplicate$ empty$
        { "there's a number but no volume in " cite$ * warning$ }
        'skip$
      if$
      swap$
      "(" swap$ * ")" *
    }
  if$ *
  eid empty$
    { format.journal.pages }
    { format.journal.eid }
  if$
}


FUNCTION {format.chapter.pages}
{ chapter empty$
    'format.pages
    { type empty$
        { bbl.chapter }
        { type "l" change.case$
          "type" bibinfo.warn
        }
      if$
      chapter tie.or.space.prefix
      "chapter" bibinfo.warn
      * *
      pages empty$
        'skip$
        { ", " * format.pages * }
      if$
    }
  if$
}


FUNCTION {format.booktitle}
{
  booktitle "booktitle" bibinfo.warn
  emphasize
}

FUNCTION {format.in.ed.booktitle}
{ format.booktitle duplicate$ empty$ 'skip$
    {
      editor "editor" format.names
      duplicate$ empty$
        'pop$
        {
          "," *
          " " *
          get.bbl.editor
          ", " *
          * swap$
          * }
      if$
      word.in swap$ *
    }
  if$
}

FUNCTION {format.thesis.type}
{ type duplicate$ empty$
    'pop$
    { swap$ pop$
      "t" change.case$ "type" bibinfo.warn
    }
  if$
}

FUNCTION {format.tr.number}
{ number "number" bibinfo.warn
  type duplicate$ empty$
    { pop$ bbl.techrep }
    'skip$
  if$
  "type" bibinfo.warn
  swap$ duplicate$ empty$
    { pop$ "t" change.case$ }
    { tie.or.space.prefix * * }
  if$
}

FUNCTION {format.article.crossref}
{
  word.in
  " \cite{" * crossref * "}" *
}

FUNCTION {format.book.crossref}
{ booktitle "booktitle" bibinfo.warn
  "in: "
  capitalize
  swap$ tie.or.space.prefix * * bbl.of space.word *
  " \cite{" * crossref * "}" *
}

FUNCTION {format.incoll.inproc.crossref}
{
  word.in
  " \cite{" * crossref * "}" *
}

FUNCTION {format.org.or.pub}
{ duplicate$ empty$
    'skip$
    {
      address missing$
        'skip$
          { address empty$
              'skip$
              { ", " address * *}
            if$
          }
      if$
    }
  if$
}

FUNCTION {format.lab.names}
{ 's :=
  "" 't :=
  s #1 "{vv~}{ll}" format.name$
  s num.names$ duplicate$
  #2 >
    { pop$
      " " * bbl.etal *
    }
    { #2 <
        'skip$
        { s #2 "{ff }{vv }{ll}{ jj}" format.name$ "others" =
            {
              " " * bbl.etal *
            }
            { bbl.and space.word * s #2 "{vv~}{ll}" format.name$
              * }
          if$
        }
      if$
    }
  if$
}

FUNCTION {format.platform}
{
  platform empty$
    { "" }
    { new.block
      platform }
  if$
}


FUNCTION {format.country}
{
  country empty$
    { "" }
    { new.block
      country }
  if$
}

FUNCTION {format.status}
{
  status empty$
    { "" }
    { new.block
      "Status: " status * }
  if$
}

FUNCTION {format.arrangement}
{
  arrangement empty$
    { "" }
    { new.block
      arrangement }
  if$
}

FUNCTION {format.section}
{
  section empty$
    { "" }
    { new.block
      section }
  if$
}

FUNCTION {format.parties}
{
  short_parties missing$
    { plaintiff empty$ defendant empty$ or
        {
          short_parties "short_parties, plaintiff, defendant" bibinfo.warn
        }
        {
          plaintiff " v " * defendant *
        }
        if$
    }
  { short_parties }
  if$
}

FUNCTION {format.counsel}
{
  counsel empty$
  skip$
  { counsel }
  if$
}

FUNCTION {format.dissent}
{
  dissent empty$
    { "" }
    { new.block
      "Dissent: " dissent * }
  if$

  concur empty$
  { "" }
  { new.block
    "Concurrence: " concur *}
  if$
  *
}

FUNCTION {format.identifier}
{
  identifier empty$
    { "" }
    { new.block
      identifier }
  if$
}

FUNCTION {format.part}
{
  part empty$
    { "" }
    { new.block
      part }
  if$
}

%%-- end format

%%-- entries

FUNCTION {article}
{ output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  crossref missing$
    {
      journal "journal" bibinfo.warn emphasize output
      format.vol.num.pages output
      year "year" bibinfo.warn format.date output
    }
    { format.article.crossref output.nonnull
      format.pages output
    }
  if$
  format.issn output
  doi "doi" bibinfo.warn format.doi output
  new.block
  format.note output
  format.eprint output
  fin.entry
}

% TODO
FUNCTION {review}
{ output.bibitem
  format.authors output
  format.key output
  new.block
  "Review of: " output
    title "title" bibinfo.warn format.title output
  new.block
  "By: " output
  bookauthor output
  new.block
  crossref missing$
    {
      journal
      "journal" bibinfo.warn
      emphasize
      output
      format.vol.num.pages output
      year "year" bibinfo.warn format.date output
    }
    { format.article.crossref output.nonnull
      format.pages output
    }
  if$
  format.issn output
  doi "doi" bibinfo.warn format.doi output
  new.block
  format.note output
  format.eprint output
  fin.entry
}

FUNCTION {book}
{ output.bibitem
  format.authors.andor.editors output
  new.block
  title "title" bibinfo.warn output
  crossref missing$
    { format.bvolume output
      new.block
      format.number.series output
      new.sentence
      publisher "publisher" bibinfo.warn format.org.or.pub output
    }
    { new.block format.book.crossref output.nonnull }
  if$
  edition format.edition output
  year "year" bibinfo.warn format.date output
  isbn "isbn" bibinfo.warn format.isbn output
  doi   "doi" bibinfo.warn format.doi output
  new.block
  format.note output
  format.eprint output
  fin.entry
}

FUNCTION {booklet}
{ output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  year "year" bibinfo.warn format.date output
  isbn "isbn" bibinfo.warn format.isbn output
  doi "doi"   bibinfo.warn format.doi output
  new.block
  format.note output
  format.eprint output
  fin.entry
}

FUNCTION {inbook}
{ output.bibitem
  format.authors.andor.editors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  crossref missing$
    { booktitle "booktitle" bibinfo.warn format.in.ed.booktitle output
      format.bvolume output
      format.chapter.pages output
      new.block
      format.number.series output
      new.sentence
      format.edition output
      isbn "isbn" bibinfo.warn format.isbn output
    }
    {
      format.chapter.pages output
      new.block
      format.book.crossref output.nonnull
    }
  if$
  year "year" bibinfo.warn format.date output
  doi "doi" bibinfo.warn format.doi output
  new.block
  format.note output
  format.eprint output
  fin.entry
}

FUNCTION {incollection}
{ output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  crossref missing$
    { booktitle "booktitle" bibinfo.warn format.in.ed.booktitle output
      format.bvolume output
      format.number.series output
      format.chapter.pages output
      new.sentence
      publisher "publisher" bibinfo.warn format.org.or.pub output
      format.edition output
      year "year" bibinfo.warn format.date output
      isbn "isbn" bibinfo.warn format.isbn output
    }
    { format.incoll.inproc.crossref output.nonnull
      format.chapter.pages output
    }
  if$
  doi "doi" bibinfo.warn format.doi output
  new.block
  format.note output
  format.eprint output
  fin.entry
}

FUNCTION {inproceedings}
{ output.bibitem
  title "title" bibinfo.warn format.title output
  new.block
  format.authors output
  format.key output
  new.block
  crossref missing$
    { format.in.ed.booktitle output
      format.bvolume output
      format.number.series output
      format.pages output
      new.sentence
      publisher empty$
        { institution output }
        { institution bibinfo.warn output
          publisher "publisher" bibinfo.warn format.org.or.pub output
        }
      if$
      year "year" bibinfo.warn format.date output
      isbn "isbn" bibinfo.warn format.isbn output
      format.issn output
    }
    { format.incoll.inproc.crossref output.nonnull
      format.pages output
    }
  if$
  doi "doi" bibinfo.warn format.doi output
  new.block
  format.note output
  format.eprint output
  fin.entry
}

FUNCTION {conference} { inproceedings }

FUNCTION {manual}
{ output.bibitem
  format.authors output
  format.key output
  new.block
  format.title  output
  institution    output
  format.edition output
  year "year" bibinfo.warn format.date output
  doi format.doi output
  new.block
  format.note output
  fin.entry
}

FUNCTION {misc}
{ output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  year "year" bibinfo.warn format.date output
  doi format.doi output
  new.block
  format.note output
  format.eprint output
  fin.entry
}

FUNCTION {thesis}
{ output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block

  % use type$ or fallback to type field
  type$ "phdthesis" =
  { bbl.phdthesis output }
  { type$ "masters" =
    { bbl.mthesis output }
    { type "phd" =
        { bbl.phdthesis output }
        { bbl.mthesis output }
      if$
    }
    if$
  }
  if$
  institution "institution" bibinfo.warn output
  year "year" bibinfo.warn format.date output
  doi "doi" bibinfo.warn format.doi output
  new.block
  format.note output
  format.eprint output
  fin.entry
}

FUNCTION {phdthesis}
{ thesis }


FUNCTION {mastersthesis}
{ thesis }


FUNCTION {proceedings}
{ output.bibitem
  format.editors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  format.bvolume output
  format.number.series output
  new.sentence
  publisher empty$
    { institution output }
    { institution "institution" bibinfo.warn output
      format.country output
    }
  if$
  year "year" bibinfo.warn format.date output
  isbn "isbn" bibinfo.warn format.isbn output
  format.issn output
  doi "doi" bibinfo.warn format.doi output
  new.block
  format.note output
  format.eprint output
  fin.entry
}

FUNCTION {report}
{ output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  format.tr.number output.nonnull
  institution "institution" bibinfo.warn output
  year "year" bibinfo.warn format.date output
  doi "doi" bibinfo.warn format.doi output
  new.block
  format.note output
  format.eprint output
  "report" output
  fin.entry
}

FUNCTION {techreport}
{ output.bibitem
  format.authors output
  format.key output
  new.block
  format.title
  output
  new.block
  format.tr.number output.nonnull
  institution "institution" bibinfo.warn output
  year "year" bibinfo.warn format.date output
  doi "doi" bibinfo.warn format.doi output
  new.block
  format.note output
  format.eprint output
  "techreport" output
  fin.entry
}

FUNCTION {unpublished}
{ output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  year "year" bibinfo.warn format.date output
  new.block
  format.note output
  format.eprint output
  fin.entry
}

FUNCTION {default.type} { misc }

FUNCTION {software}
{
  output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  year "year" bibinfo.warn format.date output
  new.block
  format.platform output
  fin.entry
}

FUNCTION {game}
{ software }

FUNCTION {documentation}
{
  output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  year "year" bibinfo.warn format.date output
  new.block
  format.platform output
  fin.entry
}

FUNCTION {case}
{
  output.bibitem
  format.parties output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  format.counsel output.check
  year              "year" bibinfo.warn format.date output
  institution       "institution" bibinfo.warn output
  format.country    output
  format.identifier output
  new.block
  fin.entry

}

FUNCTION {legal}
{ case }


FUNCTION {judicial}
{
  output.bibitem
  format.parties output
  format.authors output
  format.key output
  new.block
  format.section output
  year "year" bibinfo.warn format.date output
  institution output
  format.country output
  format.status output
  format.dissent output
  format.identifier output
  new.block
  fin.entry
}

FUNCTION {law}
{
  output.bibitem
  title "title" bibinfo.warn format.title output
  institution "institution" bibinfo.warn output
  format.key output
  format.authors output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  format.section output
  year "year" bibinfo.warn format.date output
  format.bvolume output
  format.status output.nonnull
  format.identifier output
  new.block
  fin.entry
}

FUNCTION {standard}
{
  output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  format.section output
  year "year" bibinfo.warn format.date output
  institution "institution" bibinfo.warn output
  format.identifier output
  new.block
  fin.entry
}

FUNCTION {online}
{
  output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  year "year" bibinfo.warn format.date output
  doi format.doi output
  new.block
  fin.entry
}

FUNCTION {blog}
{ online }

FUNCTION {tweet}
{ online }

FUNCTION {thread}
{ online }

FUNCTION {musicscore}
{
  output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  format.part output
  year "year" bibinfo.warn format.date output
  doi format.doi output
  format.arrangement output
  new.block
  fin.entry
}

FUNCTION {dataset}
{
  output.bibitem
  format.authors output
  format.key output
  new.block
  title "title" bibinfo.warn format.title output
  new.block
  year "year" bibinfo.warn format.date output
  doi format.doi output
  new.block
  fin.entry
}



%%-- end entries

%%-- labels
FUNCTION {author.key.label}
{ author empty$
    { key empty$
        { cite$ #1 #3 substring$ }
        'key
      if$
    }
    { author format.lab.names }
  if$
}


FUNCTION {author.editor.key.label}
{ author empty$
    { editor empty$
        { key empty$
            { cite$ #1 #3 substring$ }
            'key
          if$
        }
        { editor format.lab.names }
      if$
    }
    { author format.lab.names }
  if$
}


FUNCTION {editor.key.label}
{ editor empty$
    { key empty$
        { cite$ #1 #3 substring$ }
        'key
      if$
    }
    { editor format.lab.names }
  if$
}


FUNCTION {calc.short.authors}
{ type$ "book" =
  type$ "inbook" =
  or
    'author.editor.key.label
    { type$ "proceedings" =
        'editor.key.label
        'author.key.label
      if$
    }
  if$
  'short.list :=
}


FUNCTION {calc.label}
{ calc.short.authors
  short.list
  ", "
  *
  year duplicate$ empty$
  short.list key field.or.null = or
     { pop$ "" }
     'skip$
  if$
  *
  'label :=
}


FUNCTION {calc.short.label}
{ calc.short.authors short.list
  'label :=
}

%%-- end labels

%%-- sorting
FUNCTION {sort.format.names}
{ 's :=
  #1 'nameptr :=
  ""
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
      "{vv{ } }{ll{ }}{  ff{ }}{  jj{ }}"
      format.name$ 't :=
      nameptr #1 >
        {
          nameptr #1
          #1 + =
          numnames #4
          > and
            { "others" 't :=
              #1 'namesleft := }
            'skip$
          if$
          "   "  *
          namesleft #1 = t "others" = and
            { "zzzzz" 't := }
            'skip$
          if$
          t sortify *
        }
        { t sortify * }
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}


FUNCTION {sort.format.title}
{ 't :=
  "A " #2
    "An " #3
      "The " #4 t chop.word
    chop.word
  chop.word
  sortify
  #1 global.max$ substring$
}

FUNCTION {author.editor.sort}
{ author empty$
    { editor empty$
        { key empty$
            { "to sort, need author, editor, or key in " cite$ * warning$
              ""
            }
            { key sortify }
          if$
        }
        { editor sort.format.names }
      if$
    }
    { author sort.format.names }
  if$
}

FUNCTION {presort}
{ calc.label
  label sortify
  "    "
  *
  author.editor.sort
  #1 entry.max$ substring$
  'sort.label :=
  sort.label
  *
  "    " * year field.or.null *
  "    " * cite$ *
  #1 entry.max$ substring$
  'sort.key$ :=
}

FUNCTION {bib.sort.order}
{ sort.label
  "    "
  *
  year field.or.null sortify
  swap$
  *
  "    " * cite$ *
  #1 entry.max$ substring$
  'sort.key$ :=
  calc.short.label
}
%%-- end sorting

%%-- passes
FUNCTION {forward.pass}
{ last.label label =
    { last.extra.num #1 + 'last.extra.num :=
      last.extra.num "z" chr.to.int$ >
       { "a" chr.to.int$ 'last.extra.num :=
         last.extra.num.extended #1 + 'last.extra.num.extended :=
       }
       'skip$
      if$
      last.extra.num.extended last.extra.num.blank >
        { last.extra.num.extended int.to.chr$
          last.extra.num int.to.chr$
          * 'extra.label := }
        { last.extra.num int.to.chr$ 'extra.label := }
      if$
    }
    { "a" chr.to.int$ 'last.extra.num :=
      "" 'extra.label :=
      label 'last.label :=
    }
  if$
  number.label #1 + 'number.label :=
}

FUNCTION {reverse.pass}
{ next.extra "b" =
    { "a" 'extra.label := }
    'skip$
  if$
  extra.label 'next.extra :=
  extra.label
  duplicate$ empty$
    'skip$
    { "{\natexlab{" swap$ * "}}" * }
  if$
  'extra.label :=
}

%%-- end passes

%%-- head and tail
FUNCTION {begin.bib}
{ preamble$ empty$
    'skip$
    { preamble$ write$ newline$ }
  if$
  "\begin{thebibliography}{" number.label int.to.str$ * "}" *
  write$ newline$
  "\providecommand{\natexlab}[1]{#1}"
  write$ newline$
  "\providecommand{\url}[1]{\texttt{#1}}"
  write$ newline$
  "\providecommand{\urlprefix}{\\* URL }"
  write$ newline$
  "\expandafter\ifx\csname urlstyle\endcsname\relax"
  write$ newline$
  "  \providecommand{\doi}[1]{doi:\discretionary{}{}{#1}}\else"
  write$ newline$
  "  \providecommand{\doi}{doi:\discretionary{}{}{}\begingroup \urlstyle{rm}\Url}\fi"
  write$ newline$
  "\providecommand{\bibAnnoteFile}[1]{%"
  write$ newline$
  "  \IfFileExists{#1}{\begin{quotation}\noindent\textsc{Key:} #1\\*"
  write$ newline$
  "  \textsc{Annotation:}\ \input{#1}\end{quotation}}{}}"
  write$ newline$
  "\providecommand{\bibAnnote}[2]{%"
  write$ newline$
  "  \begin{quotation}\noindent\textsc{Key:} #1\\*"
  write$ newline$
  "  \textsc{Annotation:}\ #2\end{quotation}}"
  write$ newline$
  "\providecommand{\eprint}[2][]{\url{#2}}"
  write$ newline$
}


FUNCTION {end.bib}
{ newline$
  "\end{thebibliography}" write$ newline$
}

%%-- end head and tail

%%-- execution
%% TODO separate passes into academic, internet, software, and legal
READ
ITERATE {presort}
EXECUTE {init.extra.label.stuff}
ITERATE {forward.pass}
REVERSE {reverse.pass}
ITERATE {bib.sort.order}
SORT
EXECUTE {begin.bib}
EXECUTE {init.state.consts}
ITERATE {call.type$}
EXECUTE {end.bib}

%%-- end execution

%% End of customized bst file
%%
%% End of file `jg_custom.bst'.
