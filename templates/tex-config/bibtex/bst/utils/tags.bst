
%%-- tags
FUNCTION {tags.parse.sep}
{ "," }

FUNCTION {tags.print.sep}
{ ", "  }

FUNCTION {tags.pre}
{
  "\begin{center} \begin{lstlisting}[breaklines=true,breakatwhitespace=true,basicstyle=\small]"

}

FUNCTION {tags.post}
{
  "\end{lstlisting} \end{center}"
}

% from https://tex.stackexchange.com/questions/474656/
FUNCTION {tags.split.inner}
{
  's :=
  "" 't :=
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
  ""
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
