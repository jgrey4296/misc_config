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
    archiveprefix
    author
    booktitle
    chapter
    collaboration
    edition
    edition_year
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
    subtitle
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

%%-- localization
%%
% Each function has a name bbl.xxx where xxx is the English word.
% The language selected here is ENGLISH
FUNCTION {bbl.empty} { "" }

FUNCTION {bbl.space} { " " }

FUNCTION {bbl.no.break.space} { "~" }

FUNCTION {bbl.dash} { "-" }

FUNCTION {bbl.dbldash} { "--" }

FUNCTION {bbl.nl} { "\\" }

FUNCTION {bbl.nlnb} { "\\*" } % newline, no page break

FUNCTION {bbl.and} { "and"}

FUNCTION {bbl.etal} { "et~al." }

FUNCTION {bbl.editors} { "editors" }

FUNCTION {bbl.editor} { "editor" }

FUNCTION {bbl.edby} { "edited by" }

FUNCTION {bbl.edition} { "edition" }

FUNCTION {bbl.edition.year} { "Edition Year" }

FUNCTION {bbl.volume} { "Volume" }

FUNCTION {bbl.of} { "of" }

FUNCTION {bbl.comma} { "," }

FUNCTION {bbl.colon} { ":" }

FUNCTION {bbl.number} { "number" }

FUNCTION {bbl.nr} { "no." }

FUNCTION {bbl.in} { "in" }

FUNCTION {bbl.pages} { "pages:" }

FUNCTION {bbl.page} { "page:" }

FUNCTION {bbl.eidpp} { "pages:" }

FUNCTION {bbl.chapter} { "chapter" }

FUNCTION {bbl.techrep} { "Technical Report" }

FUNCTION {bbl.mthesis} { "Master's thesis" }

FUNCTION {bbl.phdthesis} { "Ph.D. thesis" }

FUNCTION {bbl.thesis} { "Thesis" }

FUNCTION {bbl.url.prefix} { "URL: "}

FUNCTION {bbl.doi.prefix} { "doi:"}

FUNCTION {bbl.tags.prefix} { "Tags:" }

FUNCTION {bbl.isbn.prefix} { "isbn:" }

FUNCTION {bbl.issn.prefix} { "issn:" }

FUNCTION {bbl.platform.prefix} { "Platform:" }

FUNCTION {bbl.status.prefix} { "Status:" }

FUNCTION {bbl.dissent.prefix} { "Prefix:" }

FUNCTION {bbl.concur.prefix} { "Concurrence:" }

FUNCTION {bbl.parties.infix} { " v " }

FUNCTION {bbl.collab.prefix} { "in collaboration with" }


%%-- end localization

%%-- utils
FUNCTION { debug } % {str}
% { "Debug: " swap$ * top$ }
{ pop$ }

function { debug_top } % str
{ duplicate$
  "Debug: Top of stack is " swap$ * top$
}

function { quick.print } % {str}
{
  duplicate$ "Quick Print: " swap$ * top$
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
{ duplicate$ empty$ 'skip$
    { "\emph{" swap$ * "}" * }
  if$
}

FUNCTION {capitalize} % {str}
{ "u" change.case$ "t" change.case$ }

FUNCTION {post.comma} % {str}
{ bbl.comma * }

FUNCTION {post.colon}
{ bbl.colon * }

FUNCTION {pre.space} % {str}
{ bbl.space swap$ * }

FUNCTION {post.space} % {str}
{ bbl.space * }

FUNCTION {wrap.paren} % {str}
{ "(" swap$ * ")" * }

FUNCTION {wrap.bracket} % {str}
{ "[" swap$ * "]" * }

FUNCTION {wrap.brace} % {str}
{ "{" swap$ * "}" * }

FUNCTION {wrap.space} % {str}
{ bbl.no.break.space swap$ bbl.no.break.space * * }

FUNCTION { post.nl }
{ bbl.nlnb * }

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

FUNCTION {tie.or.space.prefix} % { str }
{ duplicate$ text.length$ #3 <
    { bbl.no.break.space }
    { bbl.space }
  if$
  swap$
}

FUNCTION {field.or.null} % {str}
{ duplicate$ empty$
    { pop$ bbl.empty }
    'skip$
  if$
}

FUNCTION {field.to.str} % {key, str}
{ swap$
  duplicate$ missing$
    {
      swap$
      "Field is missing: " swap$ * " in " * cite$ * debug
      pop$
      bbl.empty
    }
    { duplicate$ empty$
        {
          swap$
          "Field is empty: " swap$ * " in " * cite$ * debug
          pop$
          bbl.empty
        }
        { swap$ pop$ }
      if$
    }
  if$
}

FUNCTION {field.to.warn} % { key str }
{ swap$
  duplicate$ missing$
    {
      swap$
      "Field is missing: " swap$ * " in " * cite$ * warning$
      pop$
      bbl.empty
    }
    { duplicate$ empty$
        {
          swap$
          "Field is empty: " swap$ * " in " * cite$ * warning$
          pop$
          bbl.empty
        }
        { swap$ pop$ }
      if$
    }
  if$
}

FUNCTION {add.blank} % { str }
{  bbl.space * before.all 'output.state := }


FUNCTION {n.dashify} % {str}
{
  't :=
  bbl.empty
    { t empty$ not }
    { t #1 #1 substring$ bbl.dash =
        { t #1 #2 substring$ bbl.dbldash = not
            { bbl.dbldash *
              t #2 global.max$ substring$ 't :=
            }
            {   { t #1 #1 substring$ bbl.dash = }
                { bbl.dash *
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

FUNCTION {either.or.check} % { str, str, str }
{ empty$
    'pop$
    { "Can't use both " swap$ * " fields in " * cite$ * warning$ }
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
      duplicate$ bbl.dash =
      swap$ duplicate$ bbl.comma =
      swap$ "+" =
      or or
        { #1 'multiresult := }
        { t #2 global.max$ substring$ 't := }
      if$
    }
  while$
  multiresult
}

FUNCTION {editor.plurality}
{
  editor num.names$ #1 >
    'bbl.editors
    'bbl.editor
  if$
}
%%-- end utils

%%-- output
FUNCTION {output.nonnull} % {str}
{ 's :=
  duplicate$ "Writing: " swap$ * debug
  output.state mid.sentence =
    { bbl.comma post.space * write$ }
    { output.state after.block =
        {
          add.period$ bbl.nlnb * write$ newline$
        }
        { output.state before.all =
            'write$
            { add.period$ bbl.space * write$ }
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

FUNCTION { output.nl } % {}
{ "Adding newline" debug
  output newline$
}

FUNCTION {output.check} % {str, errmsg}
{ 't :=
  duplicate$ empty$
    { pop$
      "Output.check: empty " t * " in " * cite$ * warning$
    }
    'output.nonnull
  if$
}

FUNCTION {entry.begin}
{ "------ Starting Item: " cite$ " of type: " type$ * * * debug
  newline$
  "\begin{minipage}{\textwidth}" write$ newline$
  "\bibitem[" year "]" * * write$
  cite$ wrap.brace write$
  newline$
  bbl.empty write$
  before.all 'output.state :=
  bbl.no.break.space
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
FUNCTION {tags.parse.sep}
{ bbl.comma }

FUNCTION {tags.print.sep}
{ bbl.comma post.space  }

FUNCTION {tags.pre.write}
{
  "\begin{center}" write$ newline$
  "\begin{lstlisting}[breaklines=true,breakatwhitespace=true,basicstyle=\small]"
  write$ newline$

}

FUNCTION {tags.post.write}
{
  "\end{lstlisting}" write$ newline$
  "\end{center}" write$ newline$
}

% from https://tex.stackexchange.com/questions/474656/
FUNCTION {tags.split.inner}
{
  's :=
  bbl.empty 't :=
  #1 'tempctra  :=
  { s empty$ not
    tempctra and }
  {s #1 #1 substring$ tags.parse.sep =
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
  bbl.empty
  #0 'tempctrb  :=
  {s empty$ not }
    { s tags.split.inner
      't :=
      's :=
      tempctrb #0 =
        { }
        { tags.print.sep * * }
      if$
      tempctrb #1 + 'tempctrb  :=
      t
    }
  while$
  *
}
FUNCTION {get.tags} % { str }
{ tags "tags" field.to.warn
  duplicate$ empty$ 'skip$
  { tags.split.outer }
  if$
}


%%-- end tags

%%-- format
FUNCTION {format.url} % { str }
{ duplicate$ empty$
    { pop$ "%% No url" }
    { "\urlprefix~\url{" swap$ * "}~" * }
  if$
}

FUNCTION {entry.finish} % { nil }
{ add.period$ write$ newline$
  url "url" field.to.warn format.url duplicate$ empty$ 'pop$
  { duplicate$ "Adding Url: " swap$ * debug
    write$ newline$
  }
  if$
  get.tags duplicate$ empty$ 'pop$
  { duplicate$ "Adding Tags: " swap$ * debug
    tags.pre.write
    bbl.tags.prefix swap$ * write$ newline$
    tags.post.write
  }
  if$
  % "\end{samepage}" write$ newline$
  "\end{minipage}" write$ newline$
  "\pagebreak[1]" write$ newline$
  "------ Finished Entry: " cite$ * debug
  % "Remaining Stack: " stack$
}

% TODO
FUNCTION {get.eprint}
{ url "url" field.to.str empty$
  doi "doi" field.to.str empty$ or not
  eprint "eprint" field.to.str empty$ not and
    { "EPrint:"
      archiveprefix "archiveprefix" field.to.str duplicate$ empty$ 'skip$
      { pre.space }
      if$ *
      eprint pre.space *
    }
    { bbl.empty }
  if$
}


FUNCTION {format.isbn} % {str}
{ duplicate$ empty$ 'skip$
    {
      new.block
      bbl.isbn.prefix swap$ *
    }
  if$
}


FUNCTION {get.issn}
{ issn duplicate$ empty$ 'skip$
  { bbl.issn.prefix swap$ * }
  if$
}


FUNCTION {format.doi} % {str}
{ duplicate$ empty$
    { pop$ "%% No Doi" }
    {"\doi" swap$ wrap.brace * }
  if$
}

INTEGERS { nameptr namesleft numnames }
STRINGS  { bibinfo}
FUNCTION {format.names}
{ 'bibinfo :=
  duplicate$ empty$ 'skip$
  {'s :=
   bbl.empty 't :=
   #1 'nameptr :=
   s num.names$ 'numnames :=
   numnames 'namesleft :=
   { namesleft #0 > }
   { s nameptr
     "{ff~}{vv~}{ll}{, jj}"
     format.name$
     bibinfo field.to.str
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
               { bbl.comma * }
               'skip$
             if$
             t "others" =
               {
                 bbl.space bbl.etal * *
               }
               {
                 bbl.and wrap.space t * *
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

FUNCTION {get.key}
{ bbl.empty }
% { author field.to.str duplicate$ empty$
%   { editor field.to.str duplicate$ empty$
%       { pop$ key field.to.str }
%       'skip$
%     if$
%   }
%   { key field.to.str }
%   if$
% }

FUNCTION {get.authors} % str
{ author "author" format.names }


FUNCTION {get.editors}
{ editor "editor" field.to.str duplicate$ empty$
    'skip$
    {
      "editor" format.names post.space
      editor.plurality wrap.paren post.comma post.space
      *
    }
  if$
}

FUNCTION {get.editors.prefixed}
{ editor "editor" field.to.str duplicate$ empty$
    'skip$
    {
      "editor" format.names post.space
      bbl.edby post.space swap$ *
    }
  if$
}

FUNCTION {get.authors.and.editors}
{ "Formatting Auths/Eds" debug
  author "author" field.to.str empty$
    { get.editors }
    { get.authors
      collaboration "collaboration" field.to.str empty$
        { bbl.empty }
        { bbl.collab.prefix post.space collaboration *
          wrap.paren pre.space
        }
        if$ *
        post.space
        get.editors.prefixed *
    }
  if$
}

FUNCTION {get.note}
{
 note missing$
 note empty$ or
    { bbl.empty }
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
      note #2 global.max$ substring$ * "note" field.to.warn
    }
  if$
}

FUNCTION {word.in}
{ bbl.in capitalize wrap.space }

% TODO format the year and republished year together
FUNCTION {format.date} % { str }
{ duplicate$ empty$
    {
      "Empty year in " cite$ * "; set to ????" * warning$
       pop$ "????"
    }
    'skip$
  if$
  extra.label *
}

FUNCTION {format.title}
{
  duplicate$ empty$ 'skip$
  {
    subtitle "subtitle" field.to.str duplicate$ empty$ 'skip$
    {
      ": " swap$ *
    }
    if$ *
    emphasize
  }
  if$
}

FUNCTION {get.bvolume}
{ volume "volume" field.to.str empty$
    { bbl.empty }
    {
      bbl.volume volume tie.or.space.prefix * *
      series "series" field.to.str duplicate$ empty$ 'pop$
      {
        swap$ bbl.of pre.space * swap$ *
      }
      if$
    }
  if$
}

FUNCTION {get.number.series}
{ volume "volume" field.to.str empty$
    { number "number" field.to.str empty$
        { series "series" field.to.str }
        { series "series" field.to.str empty$
            { number }
            { output.state mid.sentence =
                { bbl.number }
                { bbl.number capitalize }
              if$
              number tie.or.space.prefix * *
              word.in *
              series *
            }
          if$
        }
      if$
    }
    { bbl.empty }
  if$
}


FUNCTION {get.edition}
{ edition "edition" field.to.str
  duplicate$ empty$
    { pop$
      edition_year "edition year" field.to.str
      duplicate$ empty$ 'skip$
      {
        bbl.edition.year swap$ *
      }
      if$
    }
    {
      output.state mid.sentence =
        { "l" }
        { "t" }
      if$ change.case$
      "edition" field.to.warn
      bbl.space * bbl.edition *
    }
  if$
}


FUNCTION {get.pages}
{ pages "pages" field.to.str duplicate$ empty$ 'skip$
  { duplicate$ multi.page.check
      {
        bbl.pages swap$
        n.dashify
      }
      {
        bbl.page swap$
      }
    if$
    tie.or.space.prefix * *
  }
  if$
}

FUNCTION {get.journal.pages}
{
  get.pages
}

FUNCTION {get.journal.eid}
{ eid "eid" field.to.warn
  duplicate$ empty$ 'skip$
  { post.colon }
  if$
}

FUNCTION {get.vol.num.pages}
{
  volume "volume" field.to.warn
  number "number" field.to.warn
  duplicate$ empty$ 'skip$
    { wrap.paren }
  if$ *
  eid "eid" field.to.str empty$
    { get.journal.pages * }
    { get.journal.eid * }
  if$
}


FUNCTION {get.chapter.pages}
{ chapter "chapter" field.to.str empty$
    'get.pages
    {
      bbl.chapter chapter tie.or.space.prefix * *
      pages "pages" field.to.str empty$
        { bbl.empty }
        { bbl.comma post.space get.pages * }
      if$ *
    }
  if$
}


FUNCTION {get.booktitle}
{
  booktitle "booktitle" field.to.warn
}

FUNCTION {get.in.ed.booktitle}
{
  word.in
  get.editors *
  get.booktitle *
}

FUNCTION {get.thesis.type}
{ type duplicate$ empty$ 'skip$
    { swap$ pop$
      "t" change.case$ "type" field.to.warn
    }
  if$
}

FUNCTION {get.tr.number}
{ number "number" field.to.warn
  type duplicate$ empty$
    { pop$ bbl.techrep }
    'skip$
  if$
  "type" field.to.warn
  swap$ duplicate$ empty$
    { pop$ "t" change.case$ }
    { tie.or.space.prefix * * }
  if$
}

FUNCTION {get.article.crossref}
{
  word.in "\cite{" crossref "}" * * *
}

FUNCTION {get.book.crossref}
{ word.in "\cite{"  crossref  "}" * * *
}

FUNCTION {get.incoll.inproc.crossref}
{
  word.in " \cite{" crossref "}" * * *
}

FUNCTION {format.org.or.pub}
{ duplicate$ empty$
    'skip$
    {
      address "address" field.to.str duplicate$ empty$ 'skip$
      { swap$ post.comma post.space swap$ }
      if$ *
    }
  if$
}

FUNCTION {format.lab.names}
{ 's :=
  bbl.empty 't :=
  s #1 "{vv~}{ll}" format.name$
  s num.names$ duplicate$
  #2 >
    { pop$
      post.space bbl.etal *
    }
    { #2 <
        'skip$
        { s #2 "{ff }{vv }{ll}{ jj}" format.name$ "others" =
            {
              post.space bbl.etal *
            }
            { bbl.and pre.space *
              s #2 "{vv~}{ll}" format.name$
              *
            }
          if$
        }
      if$
    }
  if$
}

FUNCTION {get.platform}
{
  platform "platform" field.to.str duplicate$ empty$ 'skip$
  { bbl.platform.prefix post.space swap$ *}
  if$
}


FUNCTION {get.country}
{
  country "country" field.to.str
}

FUNCTION {get.status}
{
  status "status" field.to.str duplicate$ empty$ 'skip$
  { bbl.status.prefix post.space swap$ * }
  if$
}

FUNCTION {get.arrangement}
{
  arrangement "arrangement" field.to.str
}

FUNCTION {get.section}
{
  section "section" field.to.str
}

FUNCTION {get.parties}
{
  short_parties "short parties" field.to.str empty$
    { plaintiff "plaintiff" field.to.str empty$
      defendant "defendent" field.to.str empty$ or
        {
          title "title" field.to.warn
        }
        {
          plaintiff bbl.parties.infix defendant * *
        }
        if$
    }
  { short_parties }
  if$
}

FUNCTION {get.counsel}
{
  counsel "counsel" field.to.str
}

FUNCTION {get.dissent}
{
  dissent "dissent" field.to.str duplicate$ empty$ 'skip$
  { bbl.dissent.prefix post.space swap$ * }
  if$

  concur "concur" field.to.str duplicate$ empty$ 'skip$
  { bbl.concur.prefix post.space swap$ *}
  if$
  *
}

FUNCTION {get.identifier}
{
  identifier "id" field.to.str
}

FUNCTION {get.part}
{
  part "part" field.to.str
}

%%-- end format

%%-- entries

FUNCTION {article}
{ entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  crossref missing$
    { new.block journal "journal" field.to.warn output
      get.vol.num.pages output
    }
    { get.article.crossref output.nonnull
      get.pages output
    }
  if$
  get.issn output
  new.block doi "doi" field.to.warn format.doi output
  new.block get.eprint output
  entry.finish
}

FUNCTION {review}
{ entry.begin
  get.authors output
  new.block "Review of: " output
  title "title" field.to.warn format.title output
  bookauthor "bookauthor" field.to.str empty$ 'skip$
  { new.block "By: " bookauthor * output }
  if$
  crossref missing$
    { new.block journal "journal" field.to.warn output
      get.vol.num.pages output
    }
    { get.article.crossref output
      get.pages output
    }
  if$
  get.issn output
  new.block doi "doi" field.to.warn format.doi output
  new.block get.eprint output
  entry.finish
}

FUNCTION {book}
{ entry.begin
  get.authors.and.editors output
  new.block title "title" field.to.warn emphasize output
  crossref missing$
    { new.block get.bvolume output
      new.block get.number.series output new.sentence
      new.block publisher "publisher" field.to.warn format.org.or.pub output
    }
    { new.block get.book.crossref output }
  if$
  get.edition output
  isbn "isbn" field.to.warn format.isbn output
  new.block doi "doi" field.to.warn format.doi output
  new.block get.eprint output
  entry.finish
}

FUNCTION {booklet}
{ entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  new.block isbn "isbn" field.to.warn format.isbn output
  new.block doi "doi"   field.to.warn format.doi output
  new.block get.eprint output
  entry.finish
}

FUNCTION {inbook}
{ entry.begin
  get.authors.and.editors output
  new.block title "title" field.to.warn format.title output
  crossref missing$
    { new.block
      get.in.ed.booktitle output
      get.bvolume output
      get.chapter.pages output
      new.block get.number.series output new.sentence
      get.edition output
      isbn "isbn" field.to.warn format.isbn output
    }
    {
      get.chapter.pages output
      new.block get.book.crossref output.nonnull
    }
  if$
  new.block doi "doi" field.to.warn format.doi output
  new.block get.eprint output
  entry.finish
}

FUNCTION {incollection}
{ entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  crossref missing$
    { new.block
      get.in.ed.booktitle output
      get.bvolume output
      get.number.series output
      get.chapter.pages output new.sentence
      publisher "publisher" field.to.warn format.org.or.pub output
      get.edition output
      isbn "isbn" field.to.warn format.isbn output
    }
    { new.block get.incoll.inproc.crossref output
      get.chapter.pages output
    }
  if$
  new.block doi "doi" field.to.warn format.doi output
  new.block get.eprint output
  entry.finish
}

FUNCTION {inproceedings}
{ entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  crossref missing$
    { new.block
      get.in.ed.booktitle output
      get.bvolume output
      get.number.series output
      get.pages output new.sentence
      publisher "publisher" field.to.warn format.org.or.pub output
      isbn "isbn" field.to.str format.isbn output
      get.issn output
    }
    {
      new.block get.incoll.inproc.crossref output.nonnull
      get.pages output
    } if$
  new.block doi "doi" field.to.warn format.doi output
  new.block get.eprint output
  entry.finish
}

FUNCTION {conference} { inproceedings }

FUNCTION {manual}
{ entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title  output
  institution "institution" field.to.warn   output
  get.edition output
  new.block doi "doi" field.to.str format.doi output
  entry.finish
}

FUNCTION {misc}
{ entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  new.block doi "doi" field.to.str format.doi output
  new.block get.eprint output
  entry.finish
}

FUNCTION {thesis}
{ entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
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
  institution "institution" field.to.warn output
  new.block doi "doi" field.to.warn format.doi output
  new.block get.eprint output
  entry.finish
}

FUNCTION {phdthesis}
{ thesis }

FUNCTION {mastersthesis}
{ thesis }

FUNCTION {proceedings}
{ entry.begin
  get.editors output
  new.block title "title" field.to.warn format.title output
  new.block get.bvolume output
  get.number.series output new.sentence
  publisher empty$
    { institution output }
    { institution "institution" field.to.warn output
      get.country output
    }
  if$
  isbn "isbn" field.to.warn format.isbn output
  get.issn output
  new.block doi "doi" field.to.warn format.doi output
  new.block get.eprint output
  entry.finish
}

FUNCTION {report}
{ entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  new.block get.tr.number output
  institution "institution" field.to.warn output
  new.block doi "doi" field.to.warn format.doi output
  new.block get.eprint output new.sentence
  "report" output
  entry.finish
}

FUNCTION {techreport}
{ entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  new.block get.tr.number output
  institution "institution" field.to.warn output
  new.block doi "doi" field.to.warn format.doi output
  new.block get.eprint output new.sentence
  "techreport" output
  entry.finish
}

FUNCTION {unpublished}
{ entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  "[Unpublished]" output
  new.block get.eprint output new.sentence
  entry.finish
}

FUNCTION {default.type} { misc }

FUNCTION {software}
{
  entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  new.block get.platform output
  entry.finish
}

FUNCTION {game}
{ software }

FUNCTION {documentation}
{
  entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  new.block "Platform: " get.platform * output
  entry.finish
}

FUNCTION {case}
{
  entry.begin
  get.parties output
  new.block get.counsel "counsel" field.to.warn output
  new.block institution "institution" field.to.warn output
  new.block get.country output
  new.block get.identifier "identifier" field.to.warn output
  entry.finish
}

FUNCTION {legal}
{ case }

FUNCTION {judicial}
{
  entry.begin
  get.authors output
  new.block get.parties output
  new.block get.section output
  new.block institution "institution" field.to.warn output
  new.block get.country output
  new.block get.status output
  get.dissent output
  get.identifier output
  entry.finish
}

FUNCTION {law}
{
  entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  institution "institution" field.to.warn output
  new.block get.section output
  get.bvolume output
  new.block get.status output
  get.identifier output
  entry.finish
}

FUNCTION {standard}
{
  entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  new.block get.section output
  institution "institution" field.to.warn output
  get.identifier output
  entry.finish
}

FUNCTION {online}
{
  entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  entry.finish
}

FUNCTION {blog}
{ online }

FUNCTION {tweet}
{ online }

FUNCTION {thread}
{ online }

FUNCTION {musicscore}
{
  entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  new.block get.part output
  new.block doi "doi" field.to.str format.doi output
  get.arrangement output
  entry.finish
}

FUNCTION {dataset}
{
  entry.begin
  get.authors output
  new.block title "title" field.to.warn format.title output
  new.block doi "doi" field.to.str format.doi output
  entry.finish
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
  bbl.comma pre.space *
  year duplicate$ empty$
  short.list key field.or.null = or
     { pop$ bbl.empty }
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
  bbl.empty
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
{ author "author" field.to.str empty$
    { editor "editor" field.to.str empty$
        { key "key" field.to.str empty$
            { "To sort, need author, editor, or key in: " cite$ * warning$
              bbl.empty
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
      bbl.empty 'extra.label :=
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
  "\providecommand{\url}[1]{\discretionary{}{}{\texttt{#1}}}"
  write$ newline$
  "\providecommand{\urlprefix}{\\* " bbl.url.prefix "}" * *
  write$ newline$
  "\expandafter\ifx\csname urlstyle\endcsname\relax"
  write$ newline$
  "  \providecommand{\doi}[1]{" bbl.doi.prefix "\discretionary{}{}{#1}}\else" * *
  write$ newline$
  "  \providecommand{\doi}{" bbl.doi.prefix "\discretionary{}{}{}\begingroup \urlstyle{rm}\Url}\fi" * *
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
