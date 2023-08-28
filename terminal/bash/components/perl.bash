#!/usr/bin/env bash

jgdebug "setting PERL"
PERL5LIB=$HOME/github/MAS/cotillion/packages/prompter/prompter:"${PERL5LIB-}"
PERL5LIB=$HOME/github/MAS/cotillion/packages/prompter/prompter/mod_aspects:$PERL5LIB
PERL5LIB=$HOME/github/MAS/cotillion/packages/prompter/prompter/mod_drama:$PERL5LIB
PERL5LIB=$HOME/github/MAS/cotillion/packages/prompter/prompter/mod_control:$PERL5LIB
PERL5LIB=$HOME/github/MAS/cotillion/packages/prompter/prompter/mod_services:$PERL5LIB
