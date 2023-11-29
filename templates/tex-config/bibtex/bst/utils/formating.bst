
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
