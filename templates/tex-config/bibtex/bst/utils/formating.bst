
FUNCTION {entry.begin}
{ "------ Starting Item: " cite$ " of type: " type$ * * * warning$
  newline$
  % "\begin{samepage}" write$ newline$
  "\begin{minipage}{\textwidth}" write$ newline$
  "\bibitem[" year "]" * * write$
  "{" cite$ "}" * * write$
  newline$
  "" write$
  before.all 'output.state :=
  " "
}

FUNCTION {entry.finish} % { nil }
{ add.period$ write$ newline$
  url "url" field.to.warn format.url duplicate$ empty$ 'pop$
  { duplicate$ "Adding Url: " swap$ * debug
    write$ newline$
  }
  if$
  get.tags duplicate$ empty$ 'pop$
  { duplicate$ "Adding Tags: " swap$ * warning$
    tags.pre write$ newline$
    bbl.tags.prefix swap$ * write$ newline$
    tags.post write$
  }
  if$
  % "\end{samepage}" write$ newline$
  "\end{minipage}" write$ newline$
  "\pagebreak[1]" write$ newline$
  "------ Finished Entry: " cite$ * warning$
  "Remaining Stack: " warning$
  stack$
}


FUNCTION {article}
{ entry.begin
  get.authors output
  get.key output
  new.block
  title "title" field.to.warn format.title output
  new.block
  crossref missing$
    {
      journal "journal" field.to.warn output
      get.vol.num.pages output
      % year "year" field.to.warn format.date output
    }
    { get.article.crossref output.nonnull
      get.pages output
    }
  if$
  get.issn output
  doi "doi" field.to.warn format.doi output
  new.block
  get.note output
  get.eprint output
  entry.finish
}


FUNCTION {book}
{ entry.begin
  get.authors.andor.editors output
  new.block
  "Title: " title "title" field.to.str * warning$
  title "title" field.to.warn emphasize output
  new.block
  crossref missing$
    { get.bvolume output
      new.block
      get.number.series output
      new.sentence
      publisher "publisher" field.to.warn format.org.or.pub output
    }
    { new.block get.book.crossref output }
  if$
  get.edition output
  % year "year" field.to.warn format.date output
  isbn "isbn" field.to.warn format.isbn output
  doi   "doi" field.to.warn format.doi output
  new.block
  get.note output
  get.eprint output
  entry.finish
}


FUNCTION {inbook}
{ entry.begin
  get.authors.andor.editors output
  get.key output
  new.block
  title "title" field.to.warn format.title output
  crossref missing$
    {
      get.in.ed.booktitle output
      get.bvolume output
      get.chapter.pages output
      new.block
      get.number.series output
      new.sentence
      get.edition output
      isbn "isbn" field.to.warn format.isbn output
    }
    {
      get.chapter.pages output
      new.block
      get.book.crossref output.nonnull
    }
  if$

  % year "year" field.to.warn format.date output
  doi "doi" field.to.warn format.doi output
  new.block
  get.note output
  get.eprint output
  entry.finish
}
