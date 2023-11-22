
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

FUNCTION {bbl.edition.year}
{ "Edition Year" space.word }

FUNCTION {bbl.volume}
{ "Volume" }


FUNCTION {bbl.of}
{ "of" }

FUNCTION {bbl.comma}
{ "," }

FUNCTION {bbl.number}
{ "number" }


FUNCTION {bbl.nr}
{ "no." }


FUNCTION {bbl.in}
{ "in" }


FUNCTION {bbl.pages}
{ "pages:" }


FUNCTION {bbl.page}
{ "page:" }


FUNCTION {bbl.eidpp}
{ "pages:" }


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

FUNCTION {bbl.url.prefix}
{ "URL: "}

FUNCTION {bbl.doi.prefix}
{ "doi:" space.word }

FUNCTION {bbl.tags.prefix}
{ "Tags:" space.word }

FUNCTION {bbl.isbn.prefix}
{ "isbn:" space.word }

FUNCTION {bbl.issn.prefix}
{ "issn:" space.word }

FUNCTION {editor.plurality}
{
  editor num.names$ #1 >
    'bbl.editors
    'bbl.editor
  if$
}

%%-- end localization
