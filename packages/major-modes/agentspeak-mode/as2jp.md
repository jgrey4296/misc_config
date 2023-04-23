# BNF for AS2JavaParser.jj {#bnf-for-as2javaparser.jj align="CENTER"}

## TOKENS {#tokens align="CENTER"}

    <DEFAULT> SKIP : {
    " "
    | "\t"
    | "\n"
    | "\r"
    | <"//" (~["\n","\r"])* ("\n" | "\r" | "\r\n")?>
    | <"/*" (~["*"])* "*" ("*" | ~["*","/"] (~["*"])* "*")* "/">
    }

    // Note: i do not why, but vars must be defined before TK_BEGIN and END

    <DEFAULT> TOKEN : {
    <VAR: <UP_LETTER> (<CHAR>)*>
    }


    <DEFAULT> TOKEN : {
    <TK_TRUE: "true">
    | <TK_FALSE: "false">
    | <TK_NOT: "not">
    | <TK_NEG: "~">
    | <TK_INTDIV: "div">
    | <TK_INTMOD: "mod">
    | <TK_BEGIN: "begin">
    | <TK_END: "end">
    | <TK_LABEL_AT: "@">
    | <TK_IF: "if">
    | <TK_ELSE: "else">
    | <TK_ELIF: "elif">
    | <TK_FOR: "for">
    | <TK_WHILE: "while">
    | <TK_PAND: "|&|">
    | <TK_POR: "|||">
    | <TK_LEFT_ARROW: "<-">
    | <TK_GOAL_CONDITION: "<:">
    | <TK_RULE_SEP: ":-">
    | <TK_NS_SEP: "::">
    | <NUMBER: ["0"-"9"] (["0"-"9"])* | (["0"-"9"])* "." (["0"-"9"])+ (["e","E"] (["+","-"])? (["0"-"9"])+)? | (["0"-"9"])+ ["e","E"] (["+","-"])? (["0"-"9"])+>
    | <STRING: "\"" (~["\"","\\","\n","\r"] | "\\" (["n","t","b","r","f","\\","\'","\""] | ["0"-"7"] (["0"-"7"])? | ["0"-"3"] ["0"-"7"] ["0"-"7"]))* "\"">
    | <ATOM: (<LC_LETTER> | "." <CHAR>) (<CHAR> | "." <CHAR>)* | "\'" (~["\'"])* "\'">
    | <UNNAMEDVARID: "_" (<DIGIT>)+ (<CHAR>)*>
    | <UNNAMEDVAR: "_" (<CHAR>)*>
    | <CHAR: <LETTER> | <DIGIT> | "_">
    | <LETTER: <LC_LETTER> | <UP_LETTER>>
    | <LC_LETTER: ["a"-"z"]>
    | <UP_LETTER: ["A"-"Z"]>
    | <DIGIT: ["0"-"9"]>
    }


## NON-TERMINALS {#non-terminals align="CENTER"}

    /* AgentSpeak Grammar */

    [agent]{#prod1} ::= ( [agent_component](#prod2) )* <EOF>


    [agent_component]{#prod2} ::= ( [directive](#prod3) | [belief](#prod4) | [initial_goal](#prod5) | [plan](#prod6) )

    /* Directive */

    [directive]{#prod3} ::= "{" ( <TK_BEGIN> [directive_arguments](#prod7) "}" (
                            [agent_component](#prod2) )* "{" <TK_END> "}" |
                            [directive_arguments](#prod7) "}" )

    [directive_arguments]{#prod7} ::= <ATOM> ( "(" [terms](#prod8) ")" )? ( [list](#prod9) )?

    /* Beliefs & Rules */

    [belief]{#prod4} ::= [literal](#prod10) ( <TK_RULE_SEP> [log_expr](#prod11) )? "."

    /* Initial goals */

    [initial_goal]{#prod5} ::= "!" [literal](#prod10) "."

    /* Plan */

    [plan]{#prod6} ::= ( [plan_annotation](#prod12) )? [trigger](#prod13) ( ":"
                        [log_expr](#prod11) )? ( <TK_GOAL_CONDITION> [log_expr](#prod11) )? (
                        <TK_LEFT_ARROW> [plan_body](#prod14) )? "." ( "{" ( [plan](#prod6)
                        )* "}" )?

    [plan_annotation]{#prod12} ::= <TK_LABEL_AT> ( ( <ATOM> | <TK_BEGIN> | <TK_END> ) ([list](#prod9) )? | [list](#prod9) )

    /* Trigger */

    [trigger]{#prod13} ::= ( "+" | "-" | "^" ) ( ( "!" | "?" ) )? [literal](#prod10)

    /* Plan body */

    [plan_body]{#prod14} ::= ( [plan_body_term](#prod15) ( ";" ( [plan_body](#prod14) )? )? |
                            [statement](#prod16) ( ";" )? ( [plan_body](#prod14) )? )

    [plan_body_term]{#prod15} ::= [plan_body_factor](#prod17) ( <TK_POR> [plan_body_term](#prod15) )?

    [plan_body_factor]{#prod17} ::= [body_formula](#prod18) ( <TK_PAND> [plan_body_factor](#prod17) )?

    [statement]{#prod16} ::= ( [stmtIF](#prod19) | [stmtFOR](#prod20) | [stmtWHILE](#prod21) )

    [stmtIF]{#prod19} ::= <TK_IF> [stmtIFCommon](#prod22)

    [stmtIFCommon]{#prod22} ::= "(" [log_expr](#prod11) ")" "{" ( [stmt_body](#prod23) )? "}" (
                                <TK_ELIF> [stmtIFCommon](#prod22) | <TK_ELSE> "{" (
                                [stmt_body](#prod23) )? "}" )?

    [stmtFOR]{#prod20} ::= <TK_FOR> "(" [log_expr](#prod11) ")" "{" ( [stmt_body](#prod23))? "}"

    [stmtWHILE]{#prod21} ::= <TK_WHILE> "(" [log_expr](#prod11) ")" "{" ([stmt_body](#prod23) )? "}"

    [stmt_body]{#prod23} ::= [plan_body](#prod14)

    [body_formula]{#prod18} ::= ( ( ( "!" | "!!" ) [literal](#prod10) ) | ( ( "?" | "+" (
                                "+" | "<" | ">" )? | "-" ( "+" | "-" )? )?
                                [log_expr](#prod11) ) )

    [rule_plan_term]{#prod24} ::= "{" ( ( [plan_term_annotation](#prod25) )? [trigger](#prod13) ( ":"
                                    [log_expr](#prod11) )? ( ( ";" ( [plan_body](#prod14) )? |
                                    <TK_LEFT_ARROW> [plan_body](#prod14) ) )? | [plan_body](#prod14) (
                                    <TK_RULE_SEP> [log_expr](#prod11) )? )? "}"

    [plan_term_annotation]{#prod25} ::= <TK_LABEL_AT> ( ( <ATOM> | <TK_BEGIN> | <TK_END> ) (
                                        [list](#prod9) )? | [var](#prod26) | [list](#prod9) )

    /* Literal */

    [literal]{#prod10} ::= ( ( [namespace](#prod27) )? ( <TK_NEG> )? ( [pred](#prod28) | [var](#prod26) ) | <TK_TRUE> | <TK_FALSE> )

    [namespace]{#prod27} ::= ( <ATOM> | [var](#prod26) )? <TK_NS_SEP>

    /* Annotated Formulae */

    [pred]{#prod28} ::= ( <ATOM> | <TK_BEGIN> | <TK_END> ) ( "(" [terms](#prod8) ")")? ( [list](#prod9) )?

    /* List of terms */

    [terms]{#prod8} ::= [term](#prod29) ( "," [term](#prod29) )*

    [term]{#prod29} ::= [log_expr](#prod11)

    [list]{#prod9} ::= "[" ( [term_in_list](#prod30) ( "," [term_in_list](#prod30) )* )? ( "|" ( <VAR> | <UNNAMEDVAR> | [list](#prod9) ) )? "]"

    // term_in_list is the same as term, but log_expr/plan_body must be enclosed by "("....")" to avoid problem with |

    [term_in_list]{#prod30} ::= ( [list](#prod9) | [arithm_expr](#prod31) | [string](#prod32) | [rule_plan_term](#prod24) )

    /* logical expression */

    [log_expr]{#prod11} ::= [log_expr_trm](#prod33) ( "|" [log_expr](#prod11) )?

    [log_expr_trm]{#prod33} ::= [log_expr_factor](#prod34) ( "&" [log_expr_trm](#prod33) )?

    [log_expr_factor]{#prod34} ::= ( <TK_NOT> )? [rel_expr](#prod35)

    /* relational expression
    used in context, body and term

                [   ]  --> this method returns the VarTerm
        |   [   ]  --> returns the Literal
        |       [   ]  --> returns the ExprTerm
    */

    [rel_expr]{#prod35} ::= ( [arithm_expr](#prod31) | [string](#prod32) | [list](#prod9) |
                            [rule_plan_term](#prod24) ) ( ( "<" | "<=" | ">" | ">="
                            | "==" | "==" | "=" | "=.." ) ( [arithm_expr](#prod31)
                            | [string](#prod32) | [list](#prod9) | [rule_plan_term](#prod24) ) )?

    /* arithmetic expression */

    [arithm_expr]{#prod31} ::= [arithm_expr_trm](#prod36) ( ( "+" | "-" ) [arithm_expr_trm](#prod36) )*

    [arithm_expr_trm]{#prod36} ::= [arithm_expr_factor](#prod37) ( ( "*" | "/" | <TK_INTDIV> | <TK_INTMOD> ) [arithm_expr_factor](#prod37) )*

    [arithm_expr_factor]{#prod37} ::= [arithm_expr_simple](#prod38) ( ( "**" ) [arithm_expr_factor](#prod37) )?

    [arithm_expr_simple]{#prod38} ::= ( "+" | "-" )? ( <NUMBER> | ( "(" [log_expr](#prod11) ")" ) | [function](#prod39) )

    [function]{#prod39} ::= [literal](#prod10)

    [var]{#prod26} ::= ( <VAR> | <UNNAMEDVARID> | <UNNAMEDVAR> ) ( [list](#prod9) )?

    [string]{#prod32} ::= <STRING>
