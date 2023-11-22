
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
