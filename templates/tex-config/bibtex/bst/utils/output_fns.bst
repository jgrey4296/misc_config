
%% Post-ENTRY declarations:
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

FUNCTION { output.nl } % {}
{ "adding newline" warning$
  output
  newline$

}

FUNCTION {output.check} % {str, errmsg}
{ 't :=
  duplicate$ empty$
    { pop$ "Output.check: empty " t * " in " * cite$ * warning$ }
    'output.nonnull
  if$
}

FUNCTION {add.blank} % { str }
{  " " * before.all 'output.state := }

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
