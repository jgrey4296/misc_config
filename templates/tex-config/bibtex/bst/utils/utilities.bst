

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
{ duplicate$ empty$ 'skip$
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

FUNCTION {tie.or.space.prefix} % { str }
{ duplicate$ text.length$ #3 <
    { "~" }
    { " " }
  if$
  swap$
}

FUNCTION {field.or.null} % {str}
{ duplicate$ empty$
    { pop$ "" }
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
      ""
    }
    { duplicate$ empty$
        {
          swap$
          "Field is empty: " swap$ * " in " * cite$ * debug
          pop$
          ""
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
      ""
    }
    { duplicate$ empty$
        {
          swap$
          "Field is empty: " swap$ * " in " * cite$ * warning$
          pop$
          ""
        }
        { swap$ pop$ }
      if$
    }
  if$
}


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
