# BNF for MAS2JavaParser.jj {#bnf-for-mas2javaparser.jj align="CENTER"}

## TOKENS {#tokens align="CENTER"}

    <DEFAULT> SKIP : {
    " "
    | "\t"
    | "\n"
    | "\r"
    | <"//" (~["\n","\r"])* ("\n" | "\r" | "\r\n")?>
    | <"/*" (~["*"])* "*" ("*" | ~["*","/"] (~["*"])* "*")* "/">
    }



    <DEFAULT> TOKEN : {
    <MAS: "MAS">
    | <AGS: "agents">
    | <ENV: "environment">
    | <CONTROL: "executionControl">
    | <AT: "at">
    | <INFRA: "infrastructure">
    | <CLASSPATH: "classpath">
    | <SOURCEPATH: "aslSourcePath">
    | <ASOEE: "events">
    | <ASOEEV: "discard" | "requeue" | "retrieve">
    | <ASOIB: "intBels">
    | <ASOIBV: "sameFocus" | "newFocus">
    | <ASONRC: "nrcbp">
    | <ASOV: "verbose">
    | <DIRECTIVE: "directives">
    | <ASOSYNC: "synchronised">
    | <ASOBOOL: "true" | "false">
    | <ASAGCLASS: "agentClass">
    | <ASAGARCHCLASS: "agentArchClass">
    | <BBCLASS: "beliefBaseClass">
    | <NUMBER: ["0"-"9"] (["0"-"9"])* | (["0"-"9"])* "." (["0"-"9"])+ (["e","E"] (["+","-"])? (["0"-"9"])+)? | (["0"-"9"])+ ["e","E"] (["+","-"])? (["0"-"9"])+>
    | <STRING: "\"" (~["\"","\\","\n","\r"] | "\\" (["n","t","b","r","f","\\","\'","\""] | ["0"-"7"] (["0"-"7"])? | ["0"-"3"] ["0"-"7"] ["0"-"7"]))* "\"">
    | <ASID: <LC_LETTER> (<LETTER> | <DIGIT> | "_")*>
    | <ID: <LETTER> (<LETTER> | <DIGIT> | "_")*>
    | <PATH: ("./" | "/" | "\\" | <DRIVER>) ((<LETTER> | <DIGIT> | "_")* ("/" | "\\"))*>
    | <DRIVER: <LETTER> ":">
    | <LETTER: <LC_LETTER> | <UP_LETTER>>
    | <LC_LETTER: ["a"-"z"]>
    | <UP_LETTER: ["A"-"Z"]>
    | <DIGIT: ["0"-"9"]>
    }

## NON-TERMINALS {#non-terminals align="CENTER"}

    /* Configuration Grammar */

[mas]{#prod1}

::=

\<MAS\> \<ASID\> \"{\" [infra](#prod2) [environment](#prod3)
[control](#prod4) [agents](#prod5) [directives](#prod6)
[classpath](#prod7) [sourcepath](#prod8) \"}\"

[infra]{#prod2}

::=

( \<INFRA\> \":\" [classDef](#prod9) )?

[agents]{#prod5}

::=

( \<AGS\> \":\" ( [agent](#prod10) )+ )?

[agent]{#prod10}

::=

\<ASID\> ( [fileName](#prod11) )? [ASoptions](#prod12) (
\<ASAGARCHCLASS\> [classDef](#prod9) \| \<ASAGCLASS\> [classDef](#prod9)
\| \<BBCLASS\> [classDef](#prod9) \| \"\#\" \<NUMBER\> \| \<AT\>
\<STRING\> )\* \";\"

[fileName]{#prod11}

::=

( \<PATH\> )? \<ASID\> ( \".\" \<ASID\> )?

[classDef]{#prod9}

::=

( \<ID\> \| \<ASID\> ) ( \".\" ( \<ID\> \| \<ASID\> ) )\* ( \"(\" ( (
[parameter](#prod13) ) ( \",\" ( [parameter](#prod13) ) )\* )? \")\" )?

[parameter]{#prod13}

::=

( [classDef](#prod9) \| \<NUMBER\> \| \<STRING\> \|
[listParameters](#prod14) )

[listParameters]{#prod14}

::=

\"\[\" ( [parameter](#prod13) ) ( \",\" [parameter](#prod13) )\* \"\]\"

[ASoptions]{#prod12}

::=

( \"\[\" [procOption](#prod15) ( \",\" [procOption](#prod15) )\* \"\]\"
)?

[procOption]{#prod15}

::=

( \<ASOEE\> \"=\" \<ASOEEV\> \| \<ASOIB\> \"=\" \<ASOIBV\> \|
\<ASOSYNC\> \"=\" \<ASOBOOL\> \| \<ASONRC\> \"=\" \<NUMBER\> \| \<ASOV\>
\"=\" \<NUMBER\> \| \<ASID\> \"=\" ( \<STRING\> \| \<ASID\> \|
\<NUMBER\> \| \<ID\> \| \<ASOBOOL\> ) )

[environment]{#prod3}

::=

( \<ENV\> \":\" [classDef](#prod9) ( \<AT\> \<STRING\> )? )?

[control]{#prod4}

::=

( \<CONTROL\> \":\" [classDef](#prod9) ( \<AT\> \<STRING\> )? )?

[classpath]{#prod7}

::=

( \<CLASSPATH\> \":\" ( \<STRING\> \";\" )+ )?

[sourcepath]{#prod8}

::=

( \<SOURCEPATH\> \":\" ( \<STRING\> \";\" )+ )?

[directives]{#prod6}

::=

( \<DIRECTIVE\> \":\" ( \<ASID\> \"=\" [classDef](#prod9) \";\" )+ )?
