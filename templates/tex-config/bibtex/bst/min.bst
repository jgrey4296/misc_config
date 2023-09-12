%%
%% A Minimum .bst file for testing and practice
%%
%%
ENTRY {mytestfield} {} {}
INTEGERS { output.state before.all mid.sentence after.sentence after.block }
STRINGS { myvar }
STRINGS { s t }


FUNCTION {init.state.consts}
{ #0 'before.all :=
  #1 'mid.sentence :=
  #2 'after.sentence :=
  #3 'after.block :=
}

% stack: {str} arg: {a} -> a
FUNCTION {output.nonnull}
{ 's :=
  % if midsentence: stack,
  output.state mid.sentence =
    { ", " * write$ }
    %% if after block, start new block
    { output.state after.block =
        { add.period$ write$
          newline$
          "\newblock " write$
        }
        % if before all, write stack
        { output.state before.all =
            'write$
            % otherwise add a period and write
            { add.period$ " " * write$ }
          if$
        }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
}

FUNCTION {output}
{
  duplicate$ empty$
    { pop$ }
    'output.nonnull
  if$
}

FUNCTION {output.check}
{ 't :=
  duplicate$ empty$
    { pop$ "Output.check: empty " t * " in " * cite$ * warning$ }
    'output.nonnull
  if$
}

FUNCTION {new.sen}
{
  after.sentence 'output.state :=
}

FUNCTION {test}
{
  ""
  "val" output new.sen
  "aweg" output new.sen
  "blah" output new.sen
  write$
  newline$
}


READ
EXECUTE {init.state.consts}
EXECUTE {test}
